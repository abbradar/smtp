module Network.SMTP.Server
       ( SMTPParameters(..)
       , smtpParameters
       , ClientInfo(..)
       , smtpConnection
       , runSMTPServer
       ) where

import Prelude hiding ((.), id)
import Data.List
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import Data.Conduit
import Control.Wire
import Control.Wire.Unsafe.Event
import qualified Data.Attoparsec.ByteString.Char8 as P
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text as T
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.Conduit.List as CL
import Network.SMTP.Address (EmailAddress)
import Network.Socket (SockAddr)
import Data.Conduit.Network

import Network.SMTP.Protocol

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
                                       , checkDataReady :: !(Validate m ())
                                       , checkData :: !(Validate m BL.ByteString)
                                       }

-- | Default SMTP parameters.
smtpParameters :: Monad m => SMTPParameters m
smtpParameters = SMTPParameters { smtpDomain = "localhost"
                                , extraExts = []
                                , checkDomain = noCheck
                                , checkSender = noCheck
                                , checkRcpt = noCheck
                                , checkDataReady = noCheck
                                , checkData = noCheck
                                }
  where noCheck _ _ = return Nothing

type SMTPWire m a b = Wire () () m a b

-- | SMTP incoming session.
-- Input: either SMTP error (in case of failed parse) or incoming command
-- Returns: pair of SMTP reply and (partially filled) ClientInfo
smtpIncoming :: Monad m => SMTPParameters m
                       -> SockAddr
                       -> SMTPWire m SMTPCommand (SMTPReply, ClientInfo)
smtpIncoming (SMTPParameters {..}) addr = dSwitch $ generic
                                          ClientInfo { clientAddr = addr
                                                     , clientDomain = ""
                                                     , clientFrom = Nothing
                                                     , clientTo = []
                                                     }
  where supportedExts = [ ("PIPELINING", Nothing) -- By design, TODO: implement pipelining protection
                        , ("SMTPUTF8", Nothing) -- By design
                        ] ++ extraExts
        ok = SMTPReply CComplete

        validate :: Monad m => (ClientInfo -> Wire s e m a' b')
                           -> Validate m a
                           -> (a -> ClientInfo -> ClientInfo)
                           -> (a -> SMTPReply)
                           -> ClientInfo
                           -> a
                           -> m ((SMTPReply, ClientInfo), Event (Wire s e m a' b'))
        validate next val update msg ci a = val ci a >>= return . \case
          Just e -> ((e, ci), NoEvent)
          Nothing -> ((msg a, updinfo), Event $ next updinfo)
            where updinfo = update a ci

        generic info = mkGen_ $ liftM Right . \case
          Helo domain -> ehlo [] domain
          Ehlo domain -> ehlo supportedExts domain
          Noop -> return ((ok "OK", info), NoEvent)
          -- TODO: Replace OK with whatever in RFC
          Quit -> return ((SMTPReply CServiceClose "OK", info), Event zeroArrow)
          _ -> return ((SMTPReply CBadSequence "Bad sequence of commands", info), NoEvent)

          where ehlo exts = validate normal checkDomain (\a x -> x { clientDomain = a })
                            (\d -> ok $ buildEhloReply $ EhloReply smtpDomain (Just ("greets " <> d)) exts)
                            info

        clearMail info = info { clientFrom = Nothing
                              , clientTo = []
                              }

        newNormal = normal . clearMail

        normal info = dSwitch $ ready <|> clear <|> generic info
          where clear = mkPure_ $ \case
                  Rset -> Right ((ok "OK", info), Event $ normal $ clearMail info)
                  _ -> Left ()

                ready = mkGen_ $ \case
                  MailFrom from -> Right <$> validate transaction checkSender (\a x -> x { clientFrom = a }) (const $ ok "OK") info from
                  _ -> return $ Left ()

                transaction info = dSwitch $ mail <|> clear <|> generic info
                  where mail = mkGen_ $ \case
                          RcptTo to -> Right <$>
                                      validate transaction checkRcpt (\a x -> x { clientTo = a : clientTo x }) (const $ ok "OK") info to
                          Data | clientTo info /= [] -> Right <$>
                                                      validate newNormal checkDataReady (const id)
                                                      (const $ SMTPReply CStartInput "Start mail input; end with <CRLF>.<CRLF>")
                                                      info ()
                          _ -> return $ Left ()

-- | Parse incoming stream and feed to incoming session automaton.
processSMTP :: forall m. Monad m => SMTPParameters m
                           -> SockAddr
                           -> Conduit ByteString m SMTPReply
processSMTP pars addr = do
  yield $ SMTPReply CServiceReady $ smtpDomain pars <> " Service ready"
  awaitCmd (smtpIncoming pars addr) ""

  where awaitInput :: P.Parser a
                   -> ByteString
                   -> (ByteString -> a -> Conduit ByteString m SMTPReply)
                   -> Conduit ByteString m SMTPReply
        awaitInput tparser tinput run = wait (P.parse tparser) tinput
          where wait parser input
                  | B.null input = await >>= \case
                      Nothing -> process parser ""
                      Just "" -> wait parser ""
                      Just str -> process parser str
                  | otherwise = process parser input
                process parser str = case parser str of
                  P.Fail rest ctx (stripPrefix "Failed reading: " -> Just err) -> do
                    let code = if "argument" `elem` ctx then CArgSyntax else CCmdSyntax
                    yield $ SMTPReply code $ T.pack err
                    wait (P.parse tparser) rest
                  P.Fail _ _ err -> error $ "waitInput: error prefix invalid: " ++ err
                  P.Partial p -> wait p ""
                  P.Done rest r -> run rest r

        awaitCmd :: SMTPWire m SMTPCommand (SMTPReply, ClientInfo)
                 -> ByteString
                 -> Conduit ByteString m SMTPReply
        awaitCmd w rest = awaitInput parseCommand rest $ \rest' input -> do
            (Right (r@(SMTPReply code _), ci), w') <- lift $ stepWire w () (Right input)
            yield r
            case code of
             CServiceClose -> return ()
             CStartInput -> awaitData w' w ci rest'
             _ -> awaitCmd w' rest'

        awaitData :: SMTPWire m SMTPCommand (SMTPReply, ClientInfo)
                  -> SMTPWire m SMTPCommand (SMTPReply, ClientInfo)
                  -> ClientInfo
                  -> ByteString
                  -> Conduit ByteString m SMTPReply
        awaitData goodw badw ci rest = awaitInput parseData rest $ \rest' input -> do
          r <- lift $ checkData pars ci input
          case r of
           Just reply -> do
             yield reply
             awaitCmd badw rest'
           Nothing -> do
             yield $ SMTPReply CComplete "OK"
             awaitCmd goodw rest'

-- | SMTP connection processing conduit.
smtpConnection :: Monad m => SMTPParameters m -> SockAddr -> Conduit ByteString m ByteString
smtpConnection pars addr = processSMTP pars addr =$= CL.map (BL.toStrict . TL.encodeUtf8 . buildReply)

-- | Run simple SMTP server with given settings.
runSMTPServer :: MonadIO m => SMTPParameters m -> ServerSettings -> (forall a. m a -> IO a) -> IO ()
runSMTPServer pars server run =
  runTCPServer server $ \c ->
  run $ appSource c $$ smtpConnection pars (appSockAddr c) =$= appSink c
