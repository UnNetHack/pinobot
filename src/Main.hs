module Main ( main ) where

import Bot

import qualified Network.IRC.Bot.Core as IRC
import qualified Network.IRC.Bot.Part.Ping as IRC
import qualified Network.IRC.Bot.Part.Channels as IRC
import qualified Network.IRC.Bot.Part.NickUser as IRC
import qualified Data.Set as S
import qualified Network.IRC.Bot.BotMonad as IRC
import qualified Network.IRC.Bot.Commands as IRC
import qualified Network.IRC.Bot.Parsec as IRC
import qualified Network.IRC.Bot.Log as IRC
import qualified Data.ByteString.UTF8 as Utf8
import qualified Data.ByteString as B
import qualified Data.Text.Encoding as T
import Control.Concurrent ( threadDelay )
import Data.List ( minimumBy, sortBy, find )
import Control.Monad
import Data.Foldable ( foldlM )

monsterPart :: (IRC.BotMonad m) => m ()
monsterPart = do
    priv_msg <- IRC.privMsg
    target <- IRC.maybeZero =<< IRC.replyTo
    maybe mzero
          (\txt -> IRC.sendCommand $ (IRC.PrivMsg Nothing [target])
                                     (T.encodeUtf8 txt))
          (case T.decodeUtf8' (IRC.msg priv_msg) of
               Left _ -> Nothing
               Right text -> message text)

utf8 :: String -> B.ByteString
utf8 = Utf8.fromString

main = do
    -- Add your channel to the S.fromList part.
    (_, part) <- IRC.initChannelsPart $ S.fromList [utf8 ""]
    IRC.simpleBot
        (IRC.nullBotConf { IRC.host = "irc.freenode.org"
                         , IRC.nick = utf8 "Pinobot"
                         , IRC.commandPrefix = "@"
                         , IRC.user =
                               IRC.nullUser { IRC.username = utf8 "pino"
                                            , IRC.realname = utf8 "Pinobot Jr."
                                            , IRC.hostname = "trankesbel" } })
        [IRC.nickUserPart, part, IRC.pingPart, monsterPart]
    forever $ threadDelay 1000000


