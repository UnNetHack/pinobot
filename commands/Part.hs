{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import IRC.Types
import System.Environment

main = do
  [channel] <- getArgs
  (_, send) <- connectToBot "127.0.0.1" 27315
  send $ Part $ T.pack channel
