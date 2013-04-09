module Main where

import qualified NetHack.Imported.MonsterData as MD
import qualified Data.Text as T
import qualified Network.IRC.Bot.Core as IRC
import qualified Network.IRC.Bot.Part.Ping as IRC
import qualified Network.IRC.Bot.Part.Channels as IRC
import qualified Network.IRC.Bot.Part.NickUser as IRC
import qualified Data.Set as S
import qualified Network.IRC.Bot.BotMonad as IRC
import qualified Network.IRC.Bot.Commands as IRC
import qualified Network.IRC.Bot.Parsec as IRC
import qualified Network.IRC.Bot.Log as IRC
import NetHack.Data.Dice ( Dice( .. ) )
import Data.Maybe ( fromJust, catMaybes )
import Text.Parsec ( ParsecT, string, (<|>), try, many, anyChar, char, spaces )
import Control.Concurrent ( threadDelay )
import Data.List ( sortBy )
import Control.Monad ( forever, when )

-- Shamelessly stolen from HaskellWiki which in turn stole it from Lloyd
-- Allison:
-- http://www.csse.monash.edu.au/~lloyd/tildeStrings/Alignment/92.IPL.html
--
-- Calculates the Levenshtein distance between two lists.
dist :: Eq a => [a] -> [a] -> Int
dist a b
    = last (if lab == 0 then mainDiag
            else if lab > 0 then lowers !! (lab - 1)
                 else{- < 0 -}   uppers !! (-1 - lab))
    where mainDiag = oneDiag a b (head uppers) (-1 : head lowers)
          uppers = eachDiag a b (mainDiag : uppers) -- upper diagonals
          lowers = eachDiag b a (mainDiag : lowers) -- lower diagonals
          eachDiag a [] diags = []
          eachDiag a (bch:bs) (lastDiag:diags) = oneDiag a bs nextDiag lastDiag : eachDiag a bs diags
              where nextDiag = head (tail diags)
          oneDiag a b diagAbove diagBelow = thisdiag
              where doDiag [] b nw n w = []
                    doDiag a [] nw n w = []
                    doDiag (ach:as) (bch:bs) nw n w = me : (doDiag as bs me (tail n) (tail w))
                        where me = if ach == bch then nw else 1 + min3 (head w) nw (head n)
                    firstelt = 1 + head diagBelow
                    thisdiag = firstelt : doDiag a b firstelt diagAbove (tail diagBelow)
          lab = length a - length b
          min3 x y z = if x < y then x else min y z

distT :: T.Text -> T.Text -> Int
distT t1 t2 = dist (T.unpack $ T.toLower t1) (T.unpack $ T.toLower t2)

data Variant = Vanilla
             | UnNetHack

-- Returns the most similar monster name along with its levenshtein distance.
mostSimilarMonster :: Variant -> T.Text -> (Int, T.Text)
mostSimilarMonster variant name = head $
    sortBy (\(a1,_) (a2,_) -> a1 `compare` a2) $
        zip (map (distT name) all)
            all
  where
    all :: [T.Text]
    all = allMonsters variant

-- Returns the most similar monster name but only if parameters and results are
-- reasonably "sane".
--
-- "Sane" here being: no overly long names allowed and if the Levenshtein
-- distance is too great, then the result is discarded.
mostSimilarMonsterSane :: Variant -> T.Text -> Maybe T.Text
mostSimilarMonsterSane variant text
    | T.length text >= 50 = Nothing
    | otherwise = let (distance, result) = mostSimilarMonster variant text
                   in if distance <= 3 then Just result else Nothing

monsterFetcher :: Variant -> T.Text -> Maybe MD.Monster
monsterFetcher Vanilla = MD.monster
monsterFetcher UnNetHack = MD.unMonster

allMonsters :: Variant -> [T.Text]
allMonsters Vanilla = MD.allMonsterNames
allMonsters UnNetHack = MD.unAllMonsterNames

monsterPart :: (IRC.BotMonad m) => m ()
monsterPart = IRC.parsecPart $ do
    try $ IRC.botPrefix
    variantStr <-
        try (string "u?") <|> try (string "v?") <|> try (string "?") <|>
        try (string "")
    case variantStr of
        "u?" -> doPart UnNetHack
        "v?" -> doPart Vanilla
        "?" -> doPart UnNetHack
        "" -> return ()
  where
    doPart variant = do
      spaces
      target <- IRC.maybeZero =<< IRC.replyTo
      rawMonsterName <- many anyChar
      let monsterName = (T.strip . T.pack) rawMonsterName
      when (T.length monsterName <= 0) $ fail "Launching the missiles."
      let match = mostSimilarMonsterSane variant monsterName
          results = case match of
              Nothing -> "No such monster."
              Just mon -> ircMonsterInformation
                              (fromJust $ monsterFetcher variant mon)
      IRC.sendCommand (IRC.PrivMsg Nothing [target] results)

yesify :: Bool -> String
yesify True = "yes"
yesify False = "no"

relevantFlag :: MD.MonsterFlag -> Maybe String
relevantFlag MD.FlFly = Just "flies"
relevantFlag MD.FlSwim = Just "swims"
relevantFlag MD.FlAmorphous = Just "amorphous"
relevantFlag MD.FlTunnel = Just "tunnels"
relevantFlag MD.FlCling = Just "clings"
relevantFlag MD.FlConceal = Just "conceals"
relevantFlag MD.FlAmphibious = Just "amphibious"
relevantFlag MD.FlBreathless = Just "breathless"
relevantFlag MD.FlSeeInvis = Just "seeinvis"
relevantFlag MD.FlThickHide = Just "thick hide"
relevantFlag MD.FlRegen = Just "regenerates"
relevantFlag MD.FlUnSolid = Just "unsolid"
relevantFlag MD.FlInfravisible = Just "infravisible"
relevantFlag MD.FlCovetous = Just "covetous"
relevantFlag MD.FlMindless = Just "mindless"
relevantFlag MD.FlNoPoly = Just "nopoly"
relevantFlag MD.FlTeleport = Just "teleports"
relevantFlag MD.FlUndead = Just "undead"
relevantFlag MD.FlDemon = Just "demon"
relevantFlag _ = Nothing

ircMonsterInformation :: MD.Monster -> String
ircMonsterInformation mon =
    T.unpack (MD.moName mon) ++ " (" ++ monsymbol ++ ")" ++
    " | Spd: " ++ show (MD.moSpeed mon) ++
    " | Res: " ++ resistances (MD.moResistances mon) ++
    "| Confers: " ++ confers (MD.moConferred mon) ++
    "| MR: " ++ show (MD.moMR mon) ++
    " | Generates: " ++ generates (MD.moGenerationPlaces mon) ++
    "| AC: " ++ show (MD.moAC mon) ++
    " | Attacks: " ++ attacks (MD.moAttacks mon) ++
    " | Alignment: " ++ show (MD.moAlign mon) ++
    " | Flags: " ++ flags
  where
    generates [] = "nowhere"
    generates places = concatMap generationPlace places
    generationPlace MD.Sheol = "sheol "
    generationPlace MD.Gehennom = "gehennom "
    generationPlace MD.Dungeons = "dungeons "

    relevantFlags 
        | MD.moGenocidable mon = "genocidable":actualFlags
        | otherwise = actualFlags
    actualFlags = catMaybes $ map relevantFlag $ MD.moFlags mon
    flags
        | relevantFlags == [] = "none"
        | tail relevantFlags == [] = head relevantFlags
        | otherwise = head relevantFlags ++
                      (concatMap (\str -> ", " ++ str) $ tail relevantFlags)
    attacks [] = "none"
    attacks [attack] = attackName attack
    attacks (x:xs) = attackName x ++ concatMap (\atk -> ", " ++ attackName atk)
                                     xs
    attackName (MD.Attack atype dtype (Dice d1 d2)) =
        show d1 ++ "d" ++ show d2 ++ " " ++ attackTypeName atype ++ " " ++
        attackDamageName dtype

    attackTypeName MD.AtNone = "passive"
    attackTypeName MD.AtClaw = "claw"
    attackTypeName MD.AtBite = "bite"
    attackTypeName MD.AtKick = "kick"
    attackTypeName MD.AtButt = "butt"
    attackTypeName MD.AtTouch = "touch"
    attackTypeName MD.AtSting = "sting"
    attackTypeName MD.AtHug = "hug"
    attackTypeName MD.AtSpit = "spit"
    attackTypeName MD.AtEngulf = "engulf"
    attackTypeName MD.AtBreath = "breath"
    attackTypeName MD.AtExplode = "explode"
    attackTypeName MD.AtSuicideExplode = "suicide explode"
    attackTypeName MD.AtGaze = "gaze"
    attackTypeName MD.AtTentacle = "tentacle"
    attackTypeName MD.AtWeapon = "weapon"
    attackTypeName MD.AtCast = "cast"

    attackDamageName MD.AdPhys = "physical"
    attackDamageName MD.AdMagicMissile = "magic missile"
    attackDamageName MD.AdFire = "fire"
    attackDamageName MD.AdCold = "cold"
    attackDamageName MD.AdSleep = "sleep"
    attackDamageName MD.AdDisintegrate = "disintegrate/death"
    attackDamageName MD.AdElectricity = "shock"
    attackDamageName MD.AdStrDrain = "drain str"
    attackDamageName MD.AdAcid = "acid"
    attackDamageName MD.AdSpecial1 = "special1"
    attackDamageName MD.AdSpecial2 = "special2"
    attackDamageName MD.AdBlind = "blind"
    attackDamageName MD.AdStun = "stun"
    attackDamageName MD.AdSlow = "slow"
    attackDamageName MD.AdParalyse = "paralyze"
    attackDamageName MD.AdLevelDrain = "level drain"
    attackDamageName MD.AdMagicDrain = "magic drain"
    attackDamageName MD.AdLegs = "legwound"
    attackDamageName MD.AdStone = "petrification"
    attackDamageName MD.AdSticking = "sticky"
    attackDamageName MD.AdGoldSteal = "gold steal"
    attackDamageName MD.AdItemSteal = "item steal"
    attackDamageName MD.AdSeduce = "seduce"
    attackDamageName MD.AdTeleport = "teleport"
    attackDamageName MD.AdRust = "rust"
    attackDamageName MD.AdConfuse = "confuse"
    attackDamageName MD.AdDigest = "digest"
    attackDamageName MD.AdHeal = "heal"
    attackDamageName MD.AdWrap = "wrap"
    attackDamageName MD.AdWere = "lycanthropy"
    attackDamageName MD.AdDexDrain = "drain dex"
    attackDamageName MD.AdConDrain = "drain con"
    attackDamageName MD.AdIntDrain = "drain int"
    attackDamageName MD.AdDisease = "disease"
    attackDamageName MD.AdRot = "rot"
    attackDamageName MD.AdSex = "sex"
    attackDamageName MD.AdHallucination = "hallucination"
    attackDamageName MD.AdDeath = "Death"
    attackDamageName MD.AdPestilence = "Pestilence"
    attackDamageName MD.AdFamine = "Famine"
    attackDamageName MD.AdSlime = "slime"
    attackDamageName MD.AdDisenchant = "disenchant"
    attackDamageName MD.AdCorrode = "corrosion"
    attackDamageName MD.AdClerical = "clerical"
    attackDamageName MD.AdSpell = "spell"
    attackDamageName MD.AdRandomBreath = "random breath"
    attackDamageName MD.AdAmuletSteal = "amulet steal"
    attackDamageName MD.AdCurse = "curse"
    attackDamageName MD.AdBlink = "blink"
    attackDamageName MD.AdLevelTeleport = "level teleport"
    attackDamageName MD.AdDecapitate = "decapitate"
    attackDamageName MD.AdFreeze = "freeze"
    attackDamageName MD.AdPunisher = "punisher"

    confers [] = "nothing "
    confers xs = concatMap resistanceName xs
    resistances [] = "none "
    resistances xs = concatMap resistanceName xs
    resistanceName MD.ReFire = "fire "
    resistanceName MD.ReCold = "cold "
    resistanceName MD.ReSleep = "sleep "
    resistanceName MD.ReDisintegrate = "disintegrate "
    resistanceName MD.ReElectricity = "shock "
    resistanceName MD.RePoison = "poison "
    resistanceName MD.ReAcid = "acid "
    resistanceName MD.RePetrification = "petrification "
    resistanceName MD.ReDrain = "drain "
    resistanceName MD.ReMagic = "magic "

    monsymbol = "\x03" ++ ircColor (MD.moColor mon) ++ [MD.moSymbol mon] ++
                "\x03"
    ircColor :: MD.Color -> String
    ircColor MD.Black = "14"
    ircColor MD.Red = "5"
    ircColor MD.Blue = "2"
    ircColor MD.BrightBlue = "12"
    ircColor MD.BrightMagenta = "13"
    ircColor MD.BrightCyan = "11"
    ircColor MD.Cyan = "10"
    ircColor MD.Orange = "4"
    ircColor MD.Green = "3"
    ircColor MD.Brown = "7"
    ircColor MD.Magenta = "6"
    ircColor MD.Gray = "15"
    ircColor MD.BrightGreen = "9"
    ircColor MD.White = "0"
    ircColor MD.Yellow = "8"

main = do
    -- Add your channel to the S.fromList part.
    (_, part) <- IRC.initChannelsPart $ S.fromList [""]
    IRC.simpleBot
        (IRC.nullBotConf { IRC.host = "irc.freenode.org"
                         , IRC.nick = "Pinobot"
                         , IRC.commandPrefix = "@"
                         , IRC.user =
                               IRC.nullUser { IRC.username = "pino"
                                            , IRC.realname = "Pinobot Jr."
                                            , IRC.hostname = "trankesbel" } })
        [IRC.nickUserPart, part, IRC.pingPart, monsterPart]
    forever $ threadDelay 1000000


