{-| Implements SMTP protocol client- and server-side according to RFC 5321. -}
module Network.SMTP.Protocol
       (
       -- * Replies
         ReplyCode
       , getCode
       , SMTPReply(..)
       , smtpReply

       , pattern CServiceReady
       , pattern CServiceClose
       , pattern CComplete
       , pattern CStartInput
       , pattern CCmdSyntax
       , pattern CArgSyntax
       , pattern CBadSequence

       , buildReply
       , parseReply

       , buildData
       , parseData

       -- * Requests
       , Domain
       , RcptAddress(..)
       , SMTPCommand(..)
       , buildCommand
       , parseCommand

       -- * Specific replies
       , Extension
       , Description
       , EhloReply(..)
       , buildEhloReply
       , parseEhloReply
       ) where

import Prelude hiding (takeWhile)
import Data.Monoid
import Data.Maybe
import Control.Applicative
import Control.Monad
import Data.List (stripPrefix)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Lazy.Builder as BL
import qualified Data.Text as T
import qualified Data.Text.Lazy.Builder as TB
import qualified Data.Text.Lazy.Builder.Int as TB
import qualified Data.Text.Lazy as TL
import Data.Text.Encoding (decodeUtf8, decodeUtf8')
import qualified Data.Attoparsec.ByteString.Char8 as AP
import Data.Attoparsec.Text
import Network.SMTP.Address (EmailAddress, address)
import qualified Network.SMTP.Address as E
import Text.Domain.Parser (Domain, domain)

newtype ReplyCode = ReplyCode { getCode :: Int }
                  deriving (Show, Eq)

data SMTPReply = SMTPReply { replyCode :: !ReplyCode
                           , replyMsg :: !T.Text
                           }
               deriving (Show, Eq)

smtpReply :: ReplyCode -> String -> SMTPReply
smtpReply code str = SMTPReply code (T.pack str)

pattern CServiceReady = ReplyCode 220
pattern CServiceClose = ReplyCode 221
pattern CComplete = ReplyCode 250
pattern CStartInput = ReplyCode 354
pattern CCmdSyntax = ReplyCode 500
pattern CArgSyntax = ReplyCode 501
pattern CBadSequence = ReplyCode 503

line :: AP.Parser B.ByteString
line = do
  r <- AP.takeTill (== '\r')
  _ <- AP.string "\r\n"
  return r

parseUtf8 :: B.ByteString -> AP.Parser T.Text
parseUtf8 str = case decodeUtf8' str of
  Left _ -> fail "Invalid text encoding"
  Right res -> return res

-- | Output a reply. Multiline replies are handled according to RFC 821, Appendix E.
buildReply :: SMTPReply -> TL.Text
buildReply (SMTPReply (ReplyCode code) str)
  | code >= 100 && code < 1000 = TB.toLazyText $ br str
  | otherwise = error "buildReply: invalid reply code"
  where br (T.break (== '\n') -> (curr, next))
          | T.null next = TB.decimal code <> " " <> TB.fromText curr <> "\r\n"
          | otherwise = TB.decimal code <> "-" <> TB.fromText curr <> "\r\n" <> br (T.tail next)

-- | Parse reply string, including <CRLF>. Multiline replies are handled accordingly.
parseReply :: AP.Parser SMTPReply
parseReply = do
  code <- ReplyCode <$> AP.decimal
  c <- AP.satisfy $ AP.inClass " -"
  msg <- line >>= parseUtf8
  if c == ' '
    then return $ SMTPReply code msg
    else do
      SMTPReply code' msg' <- parseReply
      when (code /= code') $ fail "Error codes in multiline reply differ"

      return $ SMTPReply code (msg <> "\n" <> msg')

data RcptAddress = Postmaster
                 | Rcpt !EmailAddress
                 deriving (Show, Eq)

data SMTPCommand = Helo !Domain
                 | Ehlo !Domain
                 | MailFrom !(Maybe EmailAddress)
                 | RcptTo !RcptAddress
                 | Data
                 | Rset
                 | Noop
                 | Quit
                 deriving (Show, Eq)

buildCommand :: SMTPCommand -> TL.Text
buildCommand = TB.toLazyText . (<> "\r\n") . bc
  where bc (Helo dom) = "HELO " <> TB.fromText dom
        bc (Ehlo dom) = "EHLO " <> TB.fromText dom
        bc (MailFrom adr) = "MAIL FROM:<" <> maybe mempty (TB.fromText . E.toText) adr <> ">"
        bc (RcptTo adr) = "RCPT TO:<" <> conv adr <> ">"
          where conv Postmaster = "Postmaster"
                conv (Rcpt a) = TB.fromText $ E.toText a
        bc Data = "DATA"
        bc Rset = "RSET"
        bc Noop = "NOOP"
        bc Quit = "QUIT"

path :: Parser EmailAddress
path = atDom *> address
  where atDom = optional $ (char '@' >> domain) `sepBy` char ',' >> char ':'

reversePath :: Parser (Maybe EmailAddress)
reversePath = char '<' *> optional path <* char '>' <?> "reverse path"

forwardPath :: Parser RcptAddress
forwardPath = do
  _ <- char '<'
  Postmaster <$ string "Postmaster>"
    <|> Rcpt <$> path <* char '>'
  <?> "forward path"

buildData :: BL.ByteString -> BL.ByteString
buildData = BL.toLazyByteString . build
  where build "" = "."
        build (BL.break (== '\n') -> (curr, next)) = res <> BL.byteString "\r\n" <> build (BL.drop 1 next)
          where res = case BL.uncons curr of
                  Just ('.', str) -> BL.byteString ".." <> BL.lazyByteString str
                  _ -> BL.lazyByteString curr

parseData :: AP.Parser BL.ByteString
parseData = BL.toLazyByteString <$> dataline
  where dataline = dotted <|> undotted
        undotted = do
          r <- BL.byteString <$> line
          n <- dataline
          return $ r <> "\r\n" <> n
        dotted = do
          _ <- AP.char '.'
          mempty <$ AP.string "\r\n"
            <|> undotted

-- Returns parser that should be ran right after a command to produce the final output.
parseCommandU :: Parser SMTPCommand
parseCommandU = do
  cmd <- takeTill (== ' ')
  case cmd of
    "HELO" -> arg $ Helo <$> domain <|> fail "Domain expected"
    "EHLO" -> arg $ Ehlo <$> domain <|> fail "Domain expected"
    "MAIL" -> arg $ MailFrom <$> (string "FROM:" >> reversePath) <|> fail "Reverse path expected"
    "RCPT" -> arg $ RcptTo <$> (string "TO:" >> forwardPath) <|> fail "Forward path expected"
    "DATA" -> noarg Data
    "RSET" -> noarg Rset
    "NOOP" -> noarg Noop
    "QUIT" -> noarg Quit
    _ -> fail "Command not recognized"

  where arg p = (char ' ' <|> fail "Argument expected") *> p <* stoparg <?> "argument"
        noarg r = (return r) <* stoparg <?> "argument"
        stoparg = endOfInput <|> fail "No more arguments expected"

runParser :: Parser a -> T.Text -> AP.Parser a
runParser parser str = check $ parse (parser <* endOfInput) str
  where check (Fail _ ctx err) = let err' = fromMaybe err (stripPrefix "Failed reading: " err)
                                 in foldr (flip (<?>)) (fail err) ctx
        check (Partial cont') = check $ cont' ""
        check (Done _ r) = return r

parseCommand :: AP.Parser SMTPCommand
parseCommand = line >>= parseUtf8 >>= runParser parseCommandU

type Extension = B.ByteString
type Description = Maybe T.Text

data EhloReply = EhloReply !Domain !Description ![(Extension, Description)]
               deriving (Show, Eq)

buildEhloReply :: EhloReply -> T.Text
buildEhloReply (EhloReply dom greet exts) = dom <> maybeConv greet <> mconcat (map conv exts)
  where conv (ext, desc) = "\n" <> decodeUtf8 ext <> maybeConv desc
        maybeConv = maybe "" ((" " <>))

parseEhloReply :: AP.Parser EhloReply
parseEhloReply = EhloReply
                 <$> ehloDomain
                 <*> desc
                 <*> many (AP.char '\n'
                           >> (,)
                           <$> ehloKeyword
                           <*> desc
                          )
  where desc = optional (AP.char ' ') >>= maybe (return Nothing) (const $ Just <$> ehloParam)
        ehloDomain = AP.takeTill (AP.inClass " \n") >>= parseUtf8 >>= runParser domain
        ehloParam = line >>= parseUtf8
        ehloKeyword = B.cons <$> AP.satisfy alphaNum <*> AP.takeWhile (\x -> alphaNum x || x == '-')
        alphaNum x = AP.isAlpha_ascii x || AP.isDigit x
