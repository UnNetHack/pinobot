{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Bot
    ( message )
    where

import Prelude hiding ( concatMap, foldl, mapM_, foldl1 )

import qualified NetHack.Data.Variant as V
import qualified NetHack.Data.Monster as MD
import qualified NetHack.Imported.Vanilla as Vanilla
import qualified NetHack.Imported.UnNetHack as UnNetHack
import qualified NetHack.Imported.UnNetHackPlus as UnNetHackPlus
import qualified NetHack.Imported.SporkHack as SporkHack
import qualified NetHack.Imported.GruntHack as GruntHack
import qualified NetHack.Imported.Slashem as Slashem
import qualified NetHack.Imported.Brass as Brass
import qualified NetHack.Imported.Dnethack as Dnethack
import qualified NetHack.Imported.SlashemExtended as SlashemExtended
import NetHack.Data.Dice
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TL
import qualified Data.Text.Lazy.Builder as TLB
import Data.Foldable
import Data.Monoid
import Data.Maybe
import Control.Monad hiding ( mapM_ )
import Control.Monad.Trans.Writer
import Control.Applicative hiding ( (<|>), many )

import Text.Parsec
import qualified Text.Parsec.Text as T

-- Add variants here.
variants :: [V.Variant]
variants = [ Vanilla.variant
           , UnNetHack.variant
           , UnNetHackPlus.variant
           , SporkHack.variant
           , GruntHack.variant
           , Slashem.variant
           , Brass.variant
           , Dnethack.variant
           , SlashemExtended.variant ]

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
          eachDiag _ [] _ = []
          eachDiag a (_:bs) (lastDiag:diags) = oneDiag a bs nextDiag lastDiag : eachDiag a bs diags
              where nextDiag = head (tail diags)
          eachDiag _ _ _ = undefined   -- suppresses warnings
          oneDiag a b diagAbove diagBelow = thisdiag
              where doDiag [] _ _ _ _ = []
                    doDiag _ [] _ _ _ = []
                    doDiag (ach:as) (bch:bs) nw n w = me : doDiag as bs me (tail n) (tail w)
                        where me = if ach == bch then nw else 1 + min3 (head w) nw (head n)
                    firstelt = 1 + head diagBelow
                    thisdiag = firstelt : doDiag a b firstelt diagAbove (tail diagBelow)
          lab = length a - length b
          min3 x y z = if x < y then x else min y z

distT :: T.Text -> T.Text -> Int
distT t1 t2 = dist (T.unpack $ T.toLower t1) (T.unpack $ T.toLower t2)

-- Returns the most similar monster name along with its levenshtein distance.
mostSimilarMonster :: V.Variant -> T.Text -> (Int, T.Text)
mostSimilarMonster variant name =
    minimumBy (\(a1,_) (a2,_) -> a1 `compare` a2) $
        zip (map (distT name) all) all
  where
    all :: [T.Text]
    all = V.allMonsterNames variant

-- Returns the most similar monster name but only if parameters and results are
-- reasonably "sane".
--
-- "Sane" here being: no overly long names allowed and if the Levenshtein
-- distance is too great, then the result is discarded.
mostSimilarMonsterSane :: V.Variant -> T.Text -> Maybe T.Text
mostSimilarMonsterSane variant text
    | T.length text >= 50 = Nothing
    | otherwise = let (distance, result) = mostSimilarMonster variant text
                   in if distance <= 3 then Just result else Nothing

decideVariant :: T.Text -> V.Variant
decideVariant name =
    fromMaybe
        UnNetHack.variant $
        find (\var -> V.commandPrefix var == name) variants

relevantFlag :: MD.MonsterFlag -> Maybe T.Text
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
relevantFlag MD.FlVegan = Just "vegan"
relevantFlag MD.FlVegetarian = Just "vegetarian"
relevantFlag MD.FlStalk = Just "stalker"
relevantFlag MD.FlMetallivore = Just "metallivore"
relevantFlag MD.FlPoisonous = Just "poisonous"
relevantFlag _ = Nothing

showB :: Show a => a -> TL.Builder
showB = TL.fromString . show

lineMonsterInformation :: MD.Monster -> T.Text
lineMonsterInformation mon = TL.toStrict $ TL.toLazyText $
    (TL.fromText $ MD.moName mon) <> " (" <> monsymbol <> ")" <>
    " | Spd: " <> showB (MD.moSpeed mon) <>
    " | Res: " <> resistances (MD.moResistances mon) <>
    "| Confers: " <> confers (MD.moConferred mon) <>
    "| MR: " <> showB (MD.moMR mon) <>
    " | Generates: " <> generates (MD.moGenerationPlaces mon) <>
    "| AC: " <> showB (MD.moAC mon) <>
    " | Attacks: " <> attacks (MD.moAttacks mon) <>
    " | Alignment: " <> showB (MD.moAlign mon) <>
    " | Flags: " <> flags
  where
    generates :: [MD.Place] -> TL.Builder
    generates [] = "special "
    generates places = mconcat $ fmap generationPlace places

    generationPlace MD.Sheol = "sheol "
    generationPlace MD.Gehennom = "gehennom "
    generationPlace MD.Dungeons = "dungeons "
    generationPlace MD.Unique = "unique "

    isCarnivore = MD.hasFlag MD.FlCarnivore mon
    isHerbivore = MD.hasFlag MD.FlHerbivore mon
    isOmnivore = isCarnivore && isHerbivore

    relevantFlagsW :: Writer [TL.Builder] ()
    relevantFlagsW = do
        when (MD.moGenocidable mon) $ tell ["genocidable"]
        if isOmnivore
          then tell ["omnivore"]
          else do when isCarnivore $ tell ["carnivore"]
                  when isHerbivore $ tell ["herbivore"]
        mapM_ (tell . fmap TL.fromText . catMaybes . return . relevantFlag) $
            MD.moFlags mon

    relevantFlags :: [TL.Builder]
    relevantFlags = execWriter relevantFlagsW

    flags :: TL.Builder
    flags
        | null relevantFlags = "none"
        | null $ tail relevantFlags = head relevantFlags
        | otherwise = foldl1 (\accum str -> accum <> ", " <> str)
                             relevantFlags

    attacks :: [MD.Attack] -> TL.Builder
    attacks [] = "none"
    attacks [attack] = attackName attack
    attacks xs = foldl1 (\accum str -> accum <> ", " <> str)
                        (fmap attackName xs)

    attackName :: MD.Attack -> TL.Builder
    attackName (MD.Attack atype dtype (Dice d1 d2)) =
        showB d1 <> "d" <> showB d2 <> " " <> attackTypeName atype <> " " <>
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
    attackTypeName MD.AtScre = "scream"
    attackTypeName MD.AtMultiply = "multiply"
    attackTypeName MD.AtArrow = "arrow"
    attackTypeName MD.AtReach = "reach"
    attackTypeName MD.AtMirror = "mirror"
    attackTypeName MD.AtWhip = "whip"
    attackTypeName MD.AtMMagical = "mmmmagical"
    attackTypeName MD.AtReachingBite = "reaching"
    attackTypeName MD.AtLash = "lashing"
    attackTypeName MD.AtTrample = "trample"
    attackTypeName MD.AtScratch = "scratch"

    attackDamageName MD.AdCalm = "calm"
    attackDamageName MD.AdTickle = "tickle"
    attackDamageName MD.AdPoly = "poly"
    attackDamageName MD.AdBehead = "behead"
    attackDamageName MD.AdCancellation = "cancellation"
    attackDamageName MD.AdPhys = "physical"
    attackDamageName MD.AdMagicMissile = "magic missile"
    attackDamageName MD.AdFire = "fire"
    attackDamageName MD.AdCold = "cold"
    attackDamageName MD.AdSleep = "sleep"
    attackDamageName MD.AdDisintegrate = "disintegrate"
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
    attackDamageName MD.AdDrown = "drown"
    attackDamageName MD.AdShred = "shred"
    attackDamageName MD.AdJailer = "jailer"
    attackDamageName MD.AdBoulderArrow = "boulder-arrow"
    attackDamageName MD.AdBoulderArrowRandomSpread =
        "boulder-arrow-random-spread"
    attackDamageName MD.AdMultiElementCounterAttackThatAngersTons =
        "multi-elemental-counterattack-that-angers-tons"  -- wtf
    attackDamageName MD.AdPoison = "poison"
    attackDamageName MD.AdWisdom = "wisdom"
    attackDamageName MD.AdVorpal = "vorpal"
    attackDamageName MD.AdStealQuestArtifact = "steals-quest-artifact"
    attackDamageName MD.AdSpawnChaos = "spawn-chaos"
    attackDamageName MD.AdIronBall = "iron-ball"
    attackDamageName MD.AdGrow = "grow"
    attackDamageName MD.AdSilver = "silver"
    attackDamageName MD.AdAbduction = "abduction"
    attackDamageName MD.AdElementalGaze = "elemental-gaze"
    attackDamageName MD.AdAsmodeusBlood = "asmodeus-blood"
    attackDamageName MD.AdMirror = "mirror"
    attackDamageName MD.AdLeviathan = "leviathan"
    attackDamageName MD.AdUnknownPriest = "unknown-priest"
    attackDamageName MD.AdMalk = "immobilizing-destroying"
    attackDamageName MD.AdTentacle = "tentacle"
    attackDamageName MD.AdWet = "wet"
    attackDamageName MD.AdHeadSpike = "head-spike"
    attackDamageName MD.AdTele = "teleportation"
    attackDamageName MD.AdLethe = "lethe-wet"
    attackDamageName MD.AdHorn = "horn"
    attackDamageName MD.AdSolar = "solar"
    attackDamageName MD.AdEscalatingDamage = "escalating-damage"
    attackDamageName MD.AdSoul = "soul"
    attackDamageName MD.AdMist = "mist"
    attackDamageName MD.AdSuck = "suck"
    attackDamageName MD.AdDrainLuck = "drain luck"
    attackDamageName MD.AdSpore = "spores"
    attackDamageName MD.AdLava = "lava"
    attackDamageName MD.AdSunflower = "sunflowerpower"
    attackDamageName MD.AdFernExplosion = "fernxplosion"
    attackDamageName MD.AdMandrake = "mandrake-shriek"
    attackDamageName MD.AdPhysRetaliate = "retaliate"
    attackDamageName MD.AdVamp = "vampire"
    attackDamageName MD.AdWebs = "vampire-arrr"
    attackDamageName MD.AdWeeping = "levtele-drain"
    attackDamageName MD.AdGaro = "rumor-dispense"
    attackDamageName MD.AdGaroMaster = "oracle-dispense"
    attackDamageName MD.AdLoadstones = "loadstone-throw"
    attackDamageName MD.AdRemoveEngravings = "remove-engravings"

    confers :: [MD.Resistance] -> TL.Builder
    confers [] = "nothing "
    confers xs = mconcat $ fmap resistanceName xs

    resistances :: [MD.Resistance] -> TL.Builder
    resistances [] = "none "
    resistances xs = mconcat $ fmap resistanceName xs

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

    -- TODO: get rid of IRCisms from this module. We are supposed to be
    -- higher-level and not care whether we are on IRC or somewhere else.
    monsymbol = "\x03" <> ircColor (MD.moColor mon) <>
                TLB.singleton (MD.moSymbol mon) <>
                "\x03"
    ircColor :: MD.Color -> TL.Builder
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


message :: T.Text -> Maybe T.Text
message input
    | T.null input        = Nothing
    | T.head input /= '@' = Nothing
    | otherwise           = message' (T.tail input)

filterNewLines :: String -> String
filterNewLines = fmap (\ch -> if ch == '\n' || ch == '\r' then ' ' else ch)

-- substandard parsec "string" function for texts
stringT :: Stream s m Char => T.Text -> ParsecT s u m T.Text
stringT txt = T.pack <$> string (T.unpack txt)

message' :: T.Text -> Maybe T.Text
message' input =
    case runParser parser () "line" input of
        Left errmsg -> Just $ T.pack $ filterNewLines $ show errmsg
        Right okay  -> okay

  where
    parser :: T.Parser (Maybe T.Text)
    parser = do
        variantStr <- foldl
                        (\previoustry variant ->
                          previoustry <|> try (stringT $ V.commandPrefix variant))
                        (fail "") variants <|> try (stringT "")
        ignore <- try (stringT "?") <|> try (stringT "")
        if ignore == "?"
          then doPart $ decideVariant variantStr
          else return Nothing

    doPart variant = do
      spaces
      rawMonsterName <- many anyChar
      let monsterName = (T.strip . T.pack) rawMonsterName
      when (T.length monsterName <= 0) $
          fail $ ";I shall now launch the missiles that will cause " <>
                 "serious international side effects."
      return $ Just $ case mostSimilarMonsterSane variant monsterName of
          Nothing -> "No such monster."
          Just mon -> lineMonsterInformation
                          (fromJust $ V.monster variant mon)

