{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module IRC.Bot (runIRCBot) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.DeepSeq
import Control.Exception
import Control.Lens hiding ((.=))
import Control.Monad
import Data.Maybe
import Data.Serialize
import Data.Text (Text)
import qualified Data.Text as T hiding (replace)
import qualified Data.Text.Encoding as T
import qualified Data.Text.Encoding.Error as T
import Data.Time
import IRC.ConfigFile
import IRC.Socket
import IRC.Types
import qualified Network.IRC.CTCP as IRC
import qualified Network.IRC.Client as IRC
import Options.Applicative
import Pipes

-- Listens to other processes like pinobot-monsterdb to connect to us.
--
-- The mvar is for other processes to communicate to us (it will be called when
-- we receive a message from e.g. pinobot-monsterdb). The second function is
-- used to send messages back to the external processes. Anything sent to it
-- will be broadcasted. Root handler sends messages to the tchan.
listener :: MVar (IRCMessage -> IO ()) -> TChan IRCMessage -> IO ()
listener mvar chan = do
  sender <- takeMVar mvar
  -- TODO: Should likely make 127.0.0.1:27315 configurable. This is the
  -- listening address pinobot-frontend listens to let other processes connect.
  serve "127.0.0.1" "27315" $ \sock addr -> mask $ \restore -> do
    putStrLn $ "Received a local connection from: " <> show addr
    bcast <- atomically $ dupTChan chan
    -- Launch off a thread that receives messages from the connected process
    -- and sends them to IRC.
    tid <- forkIO $ restore $ do
      runEffect $
        fromSocket sock
          >-> receiveIRCMessages
          >-> (forever $ await >>= liftIO . sender)
    flip finally (killThread tid) $ restore $ forever $ do
      next_msg <- atomically $ readTChan bcast
      send sock (encode next_msg)

{-# INLINE logInfo #-}
logInfo :: MonadIO m => String -> m ()
logInfo txt = liftIO $ do
  now <- getCurrentTime
  let msg = "[" <> formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S" now <> "] " <> txt
  msg `deepseq` putStrLn msg

-- This is called by irc-client library whenever anything happens with IRC
-- connection.
rootEventHandler :: Configuration -> MVar (IRCMessage -> IO ()) -> TChan IRCMessage -> IRC.EventHandler ()
rootEventHandler _conf send_message_fun broadcast_to_external_chan = IRC.EventHandler eventText ircHandle
  where
    eventText event = Just event

    ircHandle :: IRC.Source Text -> IRC.Event Text -> IRC.IRC () ()
    ircHandle source_text event = do
      let (source_reply_to, source_nick, replyable) = case source_text of
            IRC.User nick -> (nick, nick, True)
            IRC.Channel channel nick -> (channel, nick, True)
            IRC.Server server -> (server, server, False)
      connected <- IRC.isConnected
      st <- IRC.getIRCState
      when connected $ do
        _ <- liftIO $ tryTakeMVar send_message_fun
        -- If we are connected, then we can handle messages coming from
        -- external processes like pinobot-monsterdb
        void $ liftIO $ tryPutMVar send_message_fun $ \msg ->
          case msg of
            PrivateMessage _who target _reply content ->
              IRC.runIRCAction (IRC.send (IRC.Privmsg target (Right content))) st
            Join channel ->
              IRC.runIRCAction (IRC.send (IRC.Join channel)) st
            Part channel ->
              IRC.runIRCAction (IRC.leaveChannel channel Nothing) st

      instance_conf <- liftIO $ atomically $ readTVar (st ^. IRC.instanceConfig)
      let my_nick = instance_conf ^. IRC.nick

      -- Log messages nicely, and broker off privmsgs that Pinobot should
      -- handle
      let src_desc = "[" <> show source_text <> "]"
      case event ^. IRC.message of
        IRC.Privmsg target (Right txt)
          | target == my_nick && T.pack "@" `T.isPrefixOf` txt -> do
              logInfo $ "[Direct message] " <> src_desc <> " " <> T.unpack txt
              when replyable $ do
                liftIO $
                  atomically $
                    writeTChan broadcast_to_external_chan $
                      PrivateMessage
                        { target = target,
                          who = source_nick,
                          reply = source_reply_to,
                          content = txt
                        }
        IRC.Privmsg target (Right txt)
          | target == my_nick ->
              logInfo $ "[Direct message] " <> src_desc <> " " <> T.unpack txt
        IRC.Privmsg target (Right txt) | T.pack "@" `T.isPrefixOf` txt -> do
          logInfo $ "[Message -> " <> T.unpack target <> "] " <> src_desc <> " " <> T.unpack txt
          when replyable $ do
            liftIO $
              atomically $
                writeTChan broadcast_to_external_chan $
                  PrivateMessage
                    { target = target,
                      who = source_nick,
                      reply = source_reply_to,
                      content = txt
                    }
        IRC.Privmsg target (Right txt) ->
          logInfo $ "[Message -> " <> T.unpack target <> "] " <> src_desc <> " " <> T.unpack txt
        IRC.Privmsg target (Left ctcp) -> do
          let decoded = T.decodeUtf8With (T.replace '?') (IRC.decodeCTCP ctcp)
          logInfo $ "[CTCP message -> " <> T.unpack target <> "] " <> src_desc <> " " <> T.unpack decoded
        IRC.Notice target (Right txt) -> do
          logInfo $ "[Notice -> " <> T.unpack target <> "] " <> src_desc <> " " <> T.unpack txt
        IRC.Notice target (Left txt) -> do
          let decoded = T.decodeUtf8With (T.replace '?') (IRC.decodeCTCP txt)
          logInfo $ "[Notice -> " <> T.unpack target <> "] " <> src_desc <> " " <> T.unpack decoded
        -- Reduce chatter in the log by ignoring pings
        IRC.Ping {} -> return ()
        IRC.Pong {} -> return ()
        IRC.Join channel -> do
          logInfo $ "[Join] " <> src_desc <> " " <> T.unpack channel
          liftIO $ atomically $ writeTChan broadcast_to_external_chan $ Join channel
        IRC.Part channel reason -> do
          logInfo $ "[Part] " <> src_desc <> " " <> T.unpack channel <> " " <> fromMaybe "" (fmap T.unpack reason)
          liftIO $ atomically $ writeTChan broadcast_to_external_chan $ Part channel
        IRC.Quit channel ->
          logInfo $ "[Quit] " <> src_desc <> " " <> fromMaybe "" (fmap T.unpack channel)
        IRC.Mode target is_mode_set modes mode_args ->
          logInfo $ "[Modeset -> " <> T.unpack target <> "] " <> src_desc <> " " <> "is_mode_set=" <> show is_mode_set <> " " <> show modes <> " " <> show mode_args
        IRC.Topic channel topic ->
          logInfo $ "[Topic] " <> T.unpack channel <> " " <> T.unpack topic
        IRC.Invite channel nick ->
          logInfo $ "[Invite -> " <> T.unpack channel <> "] " <> T.unpack nick
        IRC.Kick channel nick reason ->
          logInfo $ "[Kick] " <> T.unpack channel <> " " <> T.unpack nick <> " " <> fromMaybe "" (fmap T.unpack reason)
        unhandled ->
          logInfo $ "[Unknown message] " <> show unhandled

prettyPrintConfig :: Configuration -> IO ()
prettyPrintConfig conf = do
  putStrLn "-- pinobot-frontend configuration --"
  putStrLn "-- User configuration --"
  putStrLn $ "Nickname:        " <> T.unpack (nickname conf)
  putStrLn $ "Username:        " <> T.unpack (username conf)
  putStrLn $ "Realname:        " <> T.unpack (realname conf)
  putStrLn "-- Connect configuration --"
  putStrLn $ "Host:            " <> T.unpack (host conf)
  putStrLn $ "Port:            " <> show (port conf)
  putStrLn $ "TLS?             " <> show (useTLS conf)

-- Arguments passable to pinobot-frontend
data FrontendCommandArgs = FrontendCommandArgs
  { pinobotConfigTomlFile :: FilePath }
  deriving ( Eq, Ord, Show )

-- for consistency, keep in sync with parseMonsterdbCommandArgs where possible
parseFrontendCommandArgs :: Parser FrontendCommandArgs
parseFrontendCommandArgs = FrontendCommandArgs
  <$> strArgument ( value "pinobot_config.toml" <> metavar "PINOBOT-TOML-CONFIG-FILE" )

runIRCBot :: IO ()
runIRCBot = withSocketsDo $ mask $ \restore -> do
  args <- execParser $ info parseFrontendCommandArgs mempty
  let config_filepath = pinobotConfigTomlFile args
  conf <- readConfigOrExitProgram config_filepath

  prettyPrintConfig conf

  -- Launch off listener that allows other processes to connect to
  -- pinobot-frontend process.
  send_message_fun <- newEmptyMVar
  chan <- newTChanIO
  tid <- forkIO $ listener send_message_fun chan

  let connection_conf1 =
        if useTLS conf
          then IRC.tlsConnection (IRC.WithDefaultConfig (T.encodeUtf8 (host conf)) (fromIntegral $ port conf))
          else IRC.plainConnection (T.encodeUtf8 (host conf)) (fromIntegral $ port conf)

      connection_conf =
        connection_conf1
          & ( IRC.username
                .~ (username conf)
            )
            . ( IRC.realname
                  .~ (realname conf)
              )

      instance_conf = IRC.defaultInstanceConfig (nickname conf) & IRC.handlers %~ (rootEventHandler conf send_message_fun chan :)

      run = IRC.runClient connection_conf instance_conf ()

  finally (restore run) $ do
    killThread tid
