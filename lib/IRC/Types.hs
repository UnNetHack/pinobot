{-# LANGUAGE DeriveGeneric, DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module IRC.Types
    ( IRCMessage(..)
    , connectToBot
    , receiveIRCMessages )
    where

import qualified Data.Text as T
import Data.Serialize
import Data.Serialize.Text()
import Data.Monoid
import Data.Typeable
import Data.Word
import Data.IORef
import GHC.Generics
import Control.Monad
import Control.Concurrent hiding ( yield )
import Control.Concurrent.STM
import Control.Exception
import qualified Data.ByteString as B
import Pipes

import IRC.Socket

data IRCMessage =
    PrivateMessage
    { who :: !T.Text       -- ignored when sent from client to server
    , target :: !T.Text
    , reply :: !T.Text
    , content :: !T.Text }
  | Join !T.Text
  | Part !T.Text
  deriving ( Eq, Ord, Show, Read, Typeable, Generic )

instance Serialize IRCMessage

connectToBot :: T.Text -> Word16 -> IO ( IO (Maybe IRCMessage)
                                       , IRCMessage -> IO () )
connectToBot host port = mask $ \restore -> do
    ref <- newIORef () -- for finalization

    chan <- newTChanIO
    mvar <- newEmptyMVar
    tid <- forkIO $ restore $ connector chan mvar
    sendaction <- takeMVar mvar
    void $ mkWeakIORef ref $ killThread tid
    return $ ( readIORef ref >> atomically (readTChan chan)
             , \x -> readIORef ref >> sendaction x )
  where
    connector chan mvar =
        flip finally (atomically $ writeTChan chan Nothing) $
        connect (T.unpack host) (show port) $ \conn addr -> do
            putStrLn $ "Connected to " <> show addr
            lock <- newMVar ()
            putMVar mvar (\x -> withMVar lock $ \_ -> send conn $ encode x)
            runEffect $
                fromSocket conn >->
                receiveIRCMessages >->
                chansender chan

    chansender chan = forever $ do
        value <- await
        liftIO $ atomically $ writeTChan chan $ Just value

receiveIRCMessages :: MonadIO m => Pipe B.ByteString IRCMessage m ()
receiveIRCMessages = decoder first_resume
  where
    first_resume :: B.ByteString -> Result IRCMessage
    first_resume = runGetPartial get

    decoder resume = await >>= rec resume
      where
        rec resume bs = case resume bs of
            Fail _ _ -> liftIO (putStrLn "Malformed data.")
            Partial next -> decoder next
            Done x rest -> yield x >> rec first_resume rest

