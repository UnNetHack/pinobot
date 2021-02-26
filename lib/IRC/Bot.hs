{-# LANGUAGE LambdaCase, MultiWayIf, RecordWildCards, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module IRC.Bot ( runIRCBot ) where

import qualified Network.IRC.Bot as IRC
import qualified Network.IRC.Bot.Part.Ping as IRC
import qualified Network.IRC.Bot.Part.NickUser as IRC
import qualified Network.IRC.Bot.Part.Channels as IRC
import Network.IRC.Base hiding ( encode )
import Network.IRC.Commands
import qualified Data.Set as S
import qualified Data.ByteString.UTF8 as Utf8
import qualified Data.Text.Encoding as T
import qualified Data.ByteString as B
import Pipes

import Data.Maybe

import IRC.Socket
import IRC.Types

import Data.Serialize
import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Data.Foldable

utf8 :: String -> B.ByteString
utf8 = Utf8.fromString

communicatorPart :: IRC.BotMonad m
                 => MVar (IRCMessage -> IO ())
                 -> TVar (S.Set B.ByteString)
                 -> TChan IRCMessage
                 -> m ()
communicatorPart mvar tvar tchan = do
    chan <- IRC.askOutChan
    _ <- liftIO $ tryPutMVar mvar (\case
        PrivateMessage {..} ->
            writeChan chan $ IRC.toMessage $
            IRC.PrivMsg Nothing [T.encodeUtf8 target] (T.encodeUtf8 content)
        Join channel -> do
            atomically $ modifyTVar tvar (S.insert (T.encodeUtf8 channel))
            writeChan chan (joinChan $ T.encodeUtf8 channel)
        Part channel -> do
            atomically $ modifyTVar tvar (S.delete (T.encodeUtf8 channel))
            writeChan chan (part $ T.encodeUtf8 channel))

    mesg <- IRC.askMessage
    maybe_nname <- IRC.askSenderNickName
    guard (isJust maybe_nname)
    let Just nname = maybe_nname

    me <- IRC.whoami

    if | msg_command mesg == "JOIN" &&
         nname == me ->
             liftIO $ atomically $ writeTChan tchan $
                 Join $ T.decodeUtf8 $ head $ msg_params mesg
       | msg_command mesg == "PART" &&
         nname == me ->
             liftIO $ atomically $ writeTChan tchan $
                 Part $ T.decodeUtf8 $ head $ msg_params mesg
       | msg_command mesg == "PRIVMSG" -> do
            privmsg <- IRC.privMsg
            reply_to <- IRC.replyTo
            case reply_to of
                Nothing -> return ()
                Just rep ->
                    liftIO $ atomically $ writeTChan tchan $
                        PrivateMessage (T.decodeUtf8 nname)
                                       (T.decodeUtf8 $ head $
                                        IRC.receivers privmsg)
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
                fromSocket sock >->
                receiveIRCMessages >->
                (forever $ await >>= liftIO . sender)
        flip finally (killThread tid) $ restore $ forever $ do
            next_msg <- atomically $ readTChan bcast
            send sock (encode next_msg)


runIRCBot :: IO ()
runIRCBot = withSocketsDo $ mask $ \restore -> do
    mvar <- newEmptyMVar
    chan <- newTChanIO
    tid <- forkIO $ listener mvar chan
    (tvar, part) <- IRC.initChannelsPart S.empty
    (tids, _) <- IRC.simpleBot
         (IRC.nullBotConf { IRC.host = "irc.freenode.org"
                          , IRC.nick = utf8 "Pinobot"
                          , IRC.commandPrefix = "@"
                          , IRC.user =
                        IRC.nullUser { IRC.username = utf8 "pino"
                                     , IRC.realname = utf8 "Pinobot"
                                     , IRC.hostname = "trankesbel" } })
         [IRC.nickUserPart, IRC.pingPart, part, communicatorPart mvar tvar chan]
    finally (restore $ forever $ threadDelay 10000000) $ do
        killThread tid
        for_ tids killThread

