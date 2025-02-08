{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module IRC.ConfigFile
  ( Configuration(..)
  , readConfigOrExitProgram )
  where

import Control.Monad
import Data.Maybe ( fromMaybe )
import qualified Data.Text as T
import Data.Text ( Text )
import System.Exit
import qualified Toml
import qualified Toml.Schema.FromValue as Toml
import Toml.Schema.FromValue ( reqKey, optKey )

instance Toml.FromValue Configuration where
  fromValue val = Toml.parseTableFromValue tbl val
   where
    tbl = do
      nick <- reqKey "nickname"
      user <- reqKey "username"
      real <- reqKey "realname"

      host <- reqKey "host"
      port <- reqKey "port"
      use_tls <- reqKey "use_tls"

      disabled_variants <- optKey "disabled_variants"

      pure Configuration
        { nickname = nick
        , username = user
        , realname = real
        , host = host
        , port = port
        , useTLS = use_tls
        , disabledVariants = fromMaybe defaultDisabledVariants disabled_variants }

data Configuration = Configuration
  { nickname :: Text,
    username :: Text,
    realname :: Text,
    useTLS :: Bool,
    host :: Text,
    port :: Int,
    disabledVariants :: [DisabledVariantText]
  }
  deriving (Eq, Ord, Show, Read)

type DisabledVariantText = Text

defaultDisabledVariants :: [DisabledVariantText]
defaultDisabledVariants = ["UnNetHackPlus", "SlashemExtended"]

readConfigOrExitProgram :: FilePath -> IO Configuration
readConfigOrExitProgram filepath = do
  putStrLn $ "Reading configuration from " <> filepath
  conf_txt <- T.pack <$> readFile filepath
  case Toml.decode' conf_txt of
    Toml.Failure errors -> do
      putStrLn $ "Cannot parse configuration file: " <> mconcat (fmap Toml.prettyDecodeError errors)
      exitFailure
    Toml.Success warnings conf -> do
      unless (null warnings) $
        putStrLn $ "Configuration parsed with warnings: " <> mconcat (fmap Toml.prettyDecodeError warnings)
      return (conf :: Configuration)
