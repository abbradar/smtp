module Network.SMTP.Server
       ( Mail(..)
       , SMTPParameters(..)
       , smtpParameters
       , ClientInfo(..)
       , smtpConnection
       , runSMTPServer
       ) where

import Prelude hiding ((.), id)
import Data.Maybe
import Data.List
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import Data.Conduit
import Control.Wire
import qualified Data.Attoparsec.ByteString.Char8 as P
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Conduit.List as CL
import Network.SMTP.Address (EmailAddress)
import Network.Socket (SockAddr)
import Data.Conduit.Network

import Network.SMTP.Protocol

type SMTPIncoming = Either SMTPReply SMTPCommand

breakSubstring' :: B.ByteString -> B.ByteString -> (B.ByteString, B.ByteString)
breakSubstring' needle haystack = let (a, b) = B.breakSubstring needle haystack
                                  in (a, B.drop (B.length needle) b)

-- | Parses incoming SMTP commands stream. When parser error is encountered, the stream
--   is skipped to the next command.
breakSMTP :: MonadIO m => Conduit ByteString m SMTPIncoming
breakSMTP = break' cmd
  where break' parser = await >>= \case
            Nothing -> void $ parse parser ""
            Just "" -> break' parser
            Just str -> parse parser str >>= break'
        parse parser str = case parser str of
            P.Fail (breakSubstring' "\r\n" -> (cur, next)) ctx (stripPrefix "Failed reading: " -> Just err) -> do
              let code = if "argument" `elem` ctx then CArgSyntax else CCmdSyntax
              yield $ Left $ SMTPReply code $ B.pack err
              -- careful here -- empty string indicates "no more input"!
              continue next
            P.Fail _ _ err -> error $ "breakSMTP: error prefix invalid: " ++ err
            P.Partial p -> return p
            P.Done str' r -> do
              yield $ Right r
              continue str'
        cmd = P.parse parseCommand

        continue "" = return cmd
        continue str = parse cmd str

data ClientInfo = ClientInfo { clientAddr :: !SockAddr
                             , clientDomain :: !Domain
                             , clientFrom :: !(Maybe EmailAddress)
                             , clientTo :: ![RcptAddress]
                             }
                deriving (Show, Eq)

-- | SMTP processing hook. Receives current client information and
--   data to be verified, and returns 'Nothing' if it's verified,
--   or @'Just' reply@ if error happened.
type Validate m a = ClientInfo -> a -> m (Maybe SMTPReply)

-- TODO:
-- Add VRFY hook.
-- Support custom X- commands
-- | SMTP server parameters. Use ReaderT transformer to pass additional context
--   to validation hooks.
--
--   It is expected that @checkData@ hook would not only validate, but also
--   process incoming mail.
data SMTPParameters m = SMTPParameters { smtpDomain :: !Domain
                                       , extraExts :: ![(Extension, Description)]
                                       , checkDomain :: !(Validate m Domain)
                                       , checkSender :: !(Validate m (Maybe EmailAddress))
                                       , checkRcpt :: !(Validate m RcptAddress)
                                       , checkData :: !(Validate m BL.ByteString)
                                       }

-- | Default SMTP parameters.
smtpParameters :: Monad m => SMTPParameters m
smtpParameters = SMTPParameters { smtpDomain = "localhost"
                                , extraExts = []
                                , checkDomain = noCheck
                                , checkSender = noCheck
                                , checkRcpt = noCheck
                                , checkData = noCheck
                                }
  where noCheck _ _ = return Nothing

type SMTPWire m a = Wire () () m SMTPCommand a

-- | SMTP incoming session.
smtpIncoming :: Monad m => SMTPParameters m a -> SockAddr -> SMTPWire m SMTPReply
smtpIncoming (SMTPParameters {..}) addr = switchMB generic
  where -- TODO: very useful in netwire!
        switchMB :: (Monoid s, Monad m) => Wire s e m a (b, Maybe (Wire s e m a b)) -> Wire s e m a b
        switchMB w = dSwitch $ second (arr (fmap fromJust) . became isJust) . w

        supportedExts = [ ("PIPELINING", Nothing) -- By design, TODO: implement pipelining protection
                        , ("SMTPUTF8", Nothing) -- By design
                        ] ++ extraExts

        ok = SMTPReply CComplete

        validate :: Monad m =>
                   (ClientInfo -> Wire s e m a' b') ->
                   Validate m a ->
                   (a -> ClientInfo -> ClientInfo) ->
                   (a -> B.ByteString) ->
                   ClientInfo -> a ->
                   m (SMTPReply, Maybe (Wire s e m a' b'))
        validate next val update msg ci a = val ci a >>= return . \case
          Just e -> (e, Nothing)
          Nothing -> (ok $ msg a, Just $ next $ update a ci)

        generic = mkGen_ $ liftM Right . \case
          Helo domain -> ehlo [] domain
          Ehlo domain -> ehlo supportedExts domain
          Noop -> return (ok "OK", Nothing)
          Quit -> return (SMTPReply CServiceClose "OK", Just zeroArrow)
          _ -> return (SMTPReply CBadSequence "Bad sequence of commands", Nothing)

          where ehlo exts = validate normal checkDomain (\a x -> x { clientDomain = a })
                              (\d -> buildEhloReply $ EhloReply smtpDomain (Just ("greets " <> encodeUtf8 d)) exts)
                              ClientInfo { clientAddr = addr
                                         , clientDomain = ""
                                         , clientFrom = Nothing
                                         , clientTo = []
                                         }

        clearMail info = info { clientFrom = Nothing
                              , clientTo = []
                              }

        normal info = switchMB $ ready <|> clear <|> generic
          where clear = mkPure_ $ \case
                  Rset -> Right (ok "OK", Just $ normal $ clearMail info)
                  _ -> Left ()

                ready = mkGen_ $ \case
                  MailFrom from -> Right <$> validate transaction checkSender (\a x -> x { clientFrom = a }) (const "OK") info from
                  _ -> return $ Left ()

                transaction info = switchMB $ mail <|> clear <|> generic
                  where mail = mkGen_ $ \case
                          RcptTo to -> Right <$>
                                      validate transaction checkRcpt (\a x -> x { clientTo = a : clientTo x }) (const "OK") info to
                          Data dat | _clientTo info /= [] ->
                                       Right <$> validate normal checkData (const clearMail) (const "OK") info dat
                          _ -> return $ Left ()

processSMTP :: Monad m => SMTPParameters m a -> SockAddr -> Conduit SMTPIncoming m SMTPReply
processSMTP pars addr = do
  yield $ SMTPReply CServiceReady $ encodeUtf8 (smtpDomain pars) <> " Service ready"
  reply $ smtpIncoming pars addr

  where reply automata = await >>= \case
          Nothing -> return ()
          Just (Left r) -> yield r >> reply automata
          Just (Right i) -> do
            (Right r, automata') <- lift $ stepWire automata () (Right i)
            yield r
            case automata' of
             WConst (Left ()) -> return ()
             _ -> reply automata'

-- | SMTP connection processing conduit.
smtpConnection :: Monad m => SMTPParameters m a -> SockAddr -> Conduit ByteString m ByteString
smtpConnection pars addr = breakSMTP =$= processSMTP pars addr =$= CL.map buildReply

-- | Run simple SMTP server with given settings.
runSMTPServer :: SMTPParameters m -> ServerSettings -> (forall a. m a -> IO a) -> IO ()
runSMTPServer pars server run =
  runTCPServer server $ \c ->
  run $ appSource c $$ smtpConnection pars (appSockAddr c) =$= appSink c
