{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Main ( main ) where

import qualified Bot as Bot
import Control.Monad
import IRC.ConfigFile
import IRC.Types
import Options.Applicative

data MonsterdbCommandArgs = MonsterdbCommandArgs
  { pinobotConfigTomlFile :: FilePath }
  deriving ( Eq, Ord, Show )

-- for consistency, keep in sync with parseFrontendCommandArgs where possible
parseMonsterdbCommandArgs :: Parser MonsterdbCommandArgs
parseMonsterdbCommandArgs = MonsterdbCommandArgs
  <$> strArgument ( value "pinobot_config.toml" <> metavar "PINOBOT-TOML-CONFIG-FILE" )

main :: IO ()
main = do
    args <- execParser $ info parseMonsterdbCommandArgs mempty
    let config_filepath = pinobotConfigTomlFile args
    conf <- readConfigOrExitProgram config_filepath

    messager <- Bot.message conf
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

