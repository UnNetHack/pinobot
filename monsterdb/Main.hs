{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Main ( main ) where

import qualified Bot as Bot
import IRC.Types
import Control.Monad

main :: IO ()
main = do
    messager <- Bot.message
    (recv, send) <- connectToBot "127.0.0.1" 27315
    forever $ do
        thing <- recv
        case thing of
            Just (PrivateMessage {..}) ->
                messager content >>= \case
                    Just answer -> send $ PrivateMessage
                        { who = "Pinobot"
                        , target = reply
                        , reply = "Pinobot"
                        , content = answer }
                    _ -> return ()
            _ -> return ()

