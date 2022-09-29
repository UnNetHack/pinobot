{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module IRC.Bot (runIRCBot) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import qualified Data.ByteString as B
import Data.Foldable
import Data.Maybe
import Data.Serialize
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import IRC.Socket
import IRC.Types
import Network.IRC.Base hiding (encode)
import qualified Network.IRC.Bot as IRC
import qualified Network.IRC.Bot.Part.Channels as IRC
import qualified Network.IRC.Bot.Part.NickUser as IRC
import qualified Network.IRC.Bot.Part.Ping as IRC
import Network.IRC.Commands
import Pipes
import System.Environment
import System.Exit
import Toml (TomlCodec, (.=))
import qualified Toml

communicatorPart ::
  IRC.BotMonad m =>
  MVar (IRCMessage -> IO ()) ->
  TVar (S.Set B.ByteString) ->
  TChan IRCMessage ->
  m ()
communicatorPart mvar tvar tchan = do
  chan <- IRC.askOutChan
  _ <-
    liftIO $
      tryPutMVar
        mvar
        ( \case
            PrivateMessage {..} ->
              writeChan chan $
                IRC.toMessage $
                  IRC.PrivMsg Nothing [T.encodeUtf8 target] (T.encodeUtf8 content)
            Join channel -> do
              atomically $ modifyTVar tvar (S.insert (T.encodeUtf8 channel))
              writeChan chan (joinChan $ T.encodeUtf8 channel)
            Part channel -> do
              atomically $ modifyTVar tvar (S.delete (T.encodeUtf8 channel))
              writeChan chan (part $ T.encodeUtf8 channel)
        )

  mesg <- IRC.askMessage
  maybe_nname <- IRC.askSenderNickName
  guard (isJust maybe_nname)
  let nname = fromJust maybe_nname

  me <- IRC.whoami

  if
      | msg_command mesg == "JOIN"
          && nname == me ->
          liftIO $
            atomically $
              writeTChan tchan $
                Join $
                  T.decodeUtf8 $
                    head $
                      msg_params mesg
      | msg_command mesg == "PART"
          && nname == me ->
          liftIO $
            atomically $
              writeTChan tchan $
                Part $
                  T.decodeUtf8 $
                    head $
                      msg_params mesg
      | msg_command mesg == "PRIVMSG" -> do
          privmsg <- IRC.privMsg
          reply_to <- IRC.replyTo
          case reply_to of
            Nothing -> return ()
            Just rep ->
              liftIO $
                atomically $
                  writeTChan tchan $
                    PrivateMessage
                      (T.decodeUtf8 nname)
                      ( T.decodeUtf8 $
                          head $
                            IRC.receivers privmsg
                      )
                      (T.decodeUtf8 rep)
                      (T.decodeUtf8 $ IRC.msg privmsg)
      | otherwise -> return ()

listener :: MVar (IRCMessage -> IO ()) -> TChan IRCMessage -> IO ()
listener mvar chan = do
  sender <- takeMVar mvar
  serve "127.0.0.1" "27315" $ \sock _ -> mask $ \restore -> do
    bcast <- atomically $ dupTChan chan
    tid <- forkIO $ restore $ do
      runEffect $
        fromSocket sock
          >-> receiveIRCMessages
          >-> (forever $ await >>= liftIO . sender)
    flip finally (killThread tid) $ restore $ forever $ do
      next_msg <- atomically $ readTChan bcast
      send sock (encode next_msg)

data Configuration = Configuration
  { nickname :: Text,
    username :: Text,
    realname :: Text,
    hostname :: Text,
    host :: Text,
    port :: Int,
    commandPrefix :: Text
  }
  deriving (Eq, Ord, Show, Read)

configurationCodec :: TomlCodec Configuration
configurationCodec =
  Configuration
    <$> Toml.text "nickname"
    .= nickname
    <*> Toml.text "username"
    .= username
    <*> Toml.text "realname"
    .= realname
    <*> Toml.text "hostname"
    .= hostname
    <*> Toml.text "host"
    .= host
    <*> Toml.int "port"
    .= port
    <*> Toml.text "command_prefix"
    .= commandPrefix

prettyPrintConfig :: Configuration -> IO ()
prettyPrintConfig conf = do
  putStrLn "-- User configuration --"
  putStrLn $ "Nickname:        " <> T.unpack (nickname conf)
  putStrLn $ "Username:        " <> T.unpack (username conf)
  putStrLn $ "Realname:        " <> T.unpack (realname conf)
  putStrLn $ "Hostname:        " <> T.unpack (hostname conf)
  putStrLn "-- Connect configuration --"
  putStrLn $ "Host:            " <> T.unpack (host conf)
  putStrLn $ "Port:            " <> show (port conf)
  putStrLn "-- Behavior configuration --"
  putStrLn $ "Command prefix:  " <> T.unpack (commandPrefix conf)

runIRCBot :: IO ()
runIRCBot = withSocketsDo $ mask $ \restore -> do
  args <- getArgs
  let config_filepath = case args of
        [] -> "pinobot_config.toml"
        [path] -> path
        _ -> error "Only one argument accepted; which is the path to configuration file toml."
  putStrLn $ "Reading configuration from " <> config_filepath
  conf_txt <- T.pack <$> readFile config_filepath
  conf <- case Toml.decode configurationCodec conf_txt of
    Left errors -> do
      putStrLn $ "Cannot parse configuration file: " <> show errors
      exitFailure
    Right conf -> return (conf :: Configuration)
  prettyPrintConfig conf
  mvar <- newEmptyMVar
  chan <- newTChanIO
  tid <- forkIO $ listener mvar chan
  (tvar, part) <- IRC.initChannelsPart S.empty
  (tids, _) <-
    IRC.simpleBot
      ( IRC.nullBotConf
          { IRC.host = T.unpack (host conf),
            IRC.nick = T.encodeUtf8 (nickname conf),
            IRC.commandPrefix = T.unpack (commandPrefix conf),
            IRC.user =
              IRC.nullUser
                { IRC.username = T.encodeUtf8 (username conf),
                  IRC.realname = T.encodeUtf8 (realname conf),
                  IRC.hostname = T.unpack (hostname conf)
                }
          }
      )
      [IRC.nickUserPart, IRC.pingPart, part, communicatorPart mvar tvar chan]
  finally (restore $ forever $ threadDelay 10000000) $ do
    killThread tid
    for_ tids killThread
