{-# LANGUAGE RecordWildCards, OverloadedStrings #-}

module Main ( main ) where

import IRC.Types
import Control.Monad
import Control.Applicative
import SafeExecute
import qualified Data.Text as T

main :: IO ()
main = forever $ do
    (recv, send) <- connectToBot "127.0.0.1" 27315
    forever $ do
        thing <- recv
        case thing of
            Just (PrivateMessage {..}) ->
                when (T.take 2 content == "@>") $ do
                    answer <- safeExecute $ T.drop 2 content
                    send $ PrivateMessage
                           { who = "Pinobot"
                           , target = reply
                           , reply = "Pinobot"
                           , content = answer }
            _ -> return ()

