{-# LANGUAGE LambdaCase #-}

module IRC.Socket
  ( Connection()
  , IRC.Socket.connect
  , withSocketsDo
  , fromSocket
  , IRC.Socket.send
  , serve )
  where

import Control.Concurrent ( forkIO )
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import qualified Data.ByteString as B
import Network.Socket
import Network.Socket.ByteString
import Pipes

newtype Connection = Connection Socket

resolve :: MonadIO m => String -> String -> m AddrInfo
resolve host port =
    liftIO $ getAddrInfo (Just defaultHints { addrSocketType = Stream }) (Just host) (Just port) >>= \case
        (addr:_) -> return addr
        _ -> error $ "getaddrinfo(): Cannot resolve: " <> show (host, port) <> " to any address."

connect :: (MonadIO m, MonadMask m)
        => String
        -> String
        -> (Connection -> SockAddr -> m a)
        -> m a
connect host port action = mask $ \restore -> do
    addr <- resolve host port

    sock <- liftIO $ openSocket addr
    liftIO $ Network.Socket.connect sock (addrAddress addr)

    sock_addr <- liftIO $ getPeerName sock

    finally
        (restore $ action (Connection sock) sock_addr)
        (liftIO $ close sock)

serve :: (MonadIO m, MonadMask m) => String -> String -> (Connection -> SockAddr -> IO ()) -> m ()
serve host port action = mask $ \restore -> do
    addr <- resolve host port
    liftIO $ print addr
    sock <- liftIO $ openSocket addr
    liftIO $ do
      setSocketOption sock ReuseAddr 1
      bind sock $ addrAddress addr
      listen sock 100

    flip finally (liftIO $ close sock) $ restore $ forever $ do
      (client_sock, client_addr) <- liftIO $ accept sock
      void $ liftIO $ forkIO $ action (Connection client_sock) client_addr

fromSocket :: MonadIO m => Connection -> Producer B.ByteString m ()
fromSocket (Connection sock) = do
    packet <- liftIO $ recv sock 4096
    if B.length packet == 0
        then yield B.empty
        else yield packet >> fromSocket (Connection sock)

send :: MonadIO m => Connection -> B.ByteString -> m ()
send (Connection sock) payload = liftIO $ sendAll sock payload
