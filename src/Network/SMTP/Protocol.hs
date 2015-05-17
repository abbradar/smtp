{-| Implements SMTP protocol client- and server-side according to RFC 5321. -}
module Network.SMTP.Protocol
       (
       -- * Replies
         ReplyCode
       , getCode
       , SMTPReply(..)

       , pattern CServiceReady
       , pattern CServiceClose
       , pattern CComplete
       , pattern CCmdSyntax
       , pattern CArgSyntax
       , pattern CBadSequence

       , buildReply
       , parseReply

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
import qualified Data.ByteString.Builder as BL
import Data.Text.Encoding (decodeUtf8', encodeUtf8)
import Data.Text.Encoding.Error (UnicodeException(..))
import qualified Data.Attoparsec.ByteString.Char8 as AP
import Data.Attoparsec.Text
import Network.SMTP.Address (EmailAddress, address)
import qualified Network.SMTP.Address as E
import Text.Domain.Parser (Domain, domain)

newtype ReplyCode = ReplyCode { getCode :: Int }
                  deriving (Show, Eq)

data SMTPReply = SMTPReply { replyCode :: !ReplyCode
                           , replyMsg :: !B.ByteString
                           }
               deriving (Show, Eq)

pattern CServiceReady = ReplyCode 220
pattern CServiceClose = ReplyCode 221
pattern CComplete = ReplyCode 250
pattern CCmdSyntax = ReplyCode 500
pattern CArgSyntax = ReplyCode 501
pattern CBadSequence = ReplyCode 503

-- | Output a reply. Multiline replies are handled according to RFC 821, Appendix E.
buildReply :: SMTPReply -> BL.Builder
buildReply (SMTPReply (ReplyCode code) str)
  | code >= 100 && code < 1000 = br str
  | otherwise = error "buildReply: invalid reply code"
  where br (B.break (== '\n') -> (curr, next))
          | B.null next = BL.intDec code <> BL.char8 ' ' <> BL.byteString curr <> BL.byteString "\r\n"
          | otherwise = BL.intDec code <> BL.char8 '-' <> BL.byteString curr <> BL.byteString "\r\n" <> br (B.tail next)

-- | Parse reply string, including <CRLF>. Multiline replies are handled accordingly.
parseReply :: AP.Parser SMTPReply
parseReply = do
  code <- ReplyCode <$> AP.decimal
  c <- AP.satisfy $ AP.inClass " -"
  msg <- AP.takeTill (== '\r') <* AP.string "\r\n"
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
                 | Data !BL.ByteString
                 | Rset
                 | Noop
                 | Quit
                 deriving (Show, Eq)

buildData :: BL.ByteString -> BL.Builder
buildData "" = "."
buildData (BL.break (== '\n') -> (curr, next)) = res <> BL.byteString "\r\n" <> buildData (BL.drop 1 next)
  where res = case BL.uncons curr of
          Just ('.', str) -> BL.byteString ".." <> BL.lazyByteString str
          _ -> BL.lazyByteString curr

buildCommand :: SMTPCommand -> BL.Builder
buildCommand = (<> "\r\n") . bc
  where bc (Helo dom) = BL.byteString "HELO " <> enc dom
        bc (Ehlo dom) = BL.byteString "EHLO " <> enc dom
        bc (MailFrom adr) = BL.byteString "MAIL FROM:<" <> maybe mempty (enc . E.toText) adr <> BL.char8 '>'
        bc (RcptTo adr) = BL.byteString "RCPT TO:<" <> conv adr <> ">"
          where conv Postmaster = BL.byteString "Postmaster"
                conv (Rcpt a) = enc $ E.toText a
        bc (Data dat) = BL.byteString "DATA\r\n" <> buildData dat
        bc Rset = BL.byteString "RSET"
        bc Noop = BL.byteString "NOOP"
        bc Quit = BL.byteString "QUIT"

        enc = BL.byteString . encodeUtf8

parseUtf8 :: Parser a -> B.ByteString -> AP.Parser a
parseUtf8 p s = do
  t <- case decodeUtf8' s of
    Left (DecodeError e _) -> fail e
    Left (EncodeError _ _) -> error "parseUtf8: can't receive EncodeError"
    Right r -> return r
  case feed (parse (p <* endOfInput) t) "" of
    Fail _ ctx e -> let e' = fromMaybe e (stripPrefix "Failed reading: " e)
                   in foldl (<?>) (fail e') ctx
    Partial _ -> error "parseUtf8: impossible happened"
    Done _ r -> return r

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

dataParse :: AP.Parser BL.Builder
dataParse = dotted <|> undotted
  where undotted = do
          r <- BL.byteString <$> AP.takeTill (== '\r')
          _ <- AP.string "\r\n"
          n <- dataParse
          return $ r <> BL.char8 '\n' <> n
        dotted = do
          _ <- AP.char '.'
          mempty <$ AP.string "\r\n"
            <|> undotted

-- Returns parser that should be ran right after a command to produce the final output.
textCommand :: Parser (AP.Parser SMTPCommand)
textCommand = do
  cmd <- takeTill (== ' ')
  case cmd of
    "HELO" -> argPure $ Helo <$> domain <|> fail "Domain expected"
    "EHLO" -> argPure $ Ehlo <$> domain <|> fail "Domain expected"
    "MAIL" -> argPure $ MailFrom <$> (string "FROM:" >> reversePath) <|> fail "Reverse path expected"
    "RCPT" -> argPure $ RcptTo <$> (string "TO:" >> forwardPath) <|> fail "Forward path expected"
    "DATA" -> stoparg >> return (Data <$> BL.toLazyByteString <$> dataParse <|> fail "Data expected")
    "RSET" -> noargPure Rset
    "NOOP" -> noargPure Noop
    "QUIT" -> noargPure Quit
    _ -> fail "Command not recognized"

  where argPure p = (char ' ' <|> fail "Argument expected") *> (return <$> p) <* stoparg <?> "argument"
        noargPure r = (return (return r)) <* stoparg <?> "argument"
        stoparg = endOfInput <|> fail "No more arguments expected"

-- | Parse an SMTP command with the following CRLF.
-- | In the case of an error, if "argument" is in contexts, error 501 must be used instead of error 500.
-- | DATA is parsed accordingly.
parseCommand :: AP.Parser SMTPCommand
parseCommand = do
  cmd <- AP.takeTill (== '\r')
  r <- parseUtf8 textCommand cmd
  _ <- AP.string "\r\n" <|> fail "CRLF expected"
  r

type Extension = B.ByteString
type Description = Maybe B.ByteString

data EhloReply = EhloReply !Domain !Description ![(Extension, Description)]
               deriving (Show, Eq)

buildEhloReply :: EhloReply -> B.ByteString
buildEhloReply (EhloReply dom greet exts) = encodeUtf8 dom <> maybe "" (" " <>) greet <> mconcat (map conv exts)
  where conv (ext, desc) = "\n" <> ext <> maybe "" (" " <>) desc

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
        ehloDomain = AP.takeTill (AP.inClass " \n") >>= parseUtf8 domain
        ehloParam = AP.takeWhile $ AP.inClass "\33-\126"
        ehloKeyword = B.cons <$> AP.satisfy alphaNum <*> AP.takeWhile (\x -> alphaNum x || x == '-')
        alphaNum x = AP.isAlpha_ascii x || AP.isDigit x
