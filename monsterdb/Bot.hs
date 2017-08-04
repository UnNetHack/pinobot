{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Bot
    ( message )
    where

import Prelude hiding ( concatMap, foldl, mapM_, foldl1 )

import qualified NetHack.Data.Variant as V
import qualified NetHack.Data.Monster as MD
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
variants :: IO [V.Variant]
variants = sequence $ variantify $
    [ "UnNetHack"        -- first one is used by default
    , "Vanilla"
    , "Vanilla343"
    , "UnNetHackPlus"
    , "SporkHack"
    , "GruntHack"
    , "Slashem"
    , "Brass"
    , "Dnethack"
    , "SlashemExtended"
    , "SlashTHEM"
    , "Fourk" ]
  where
    variantify = fmap $ \name -> V.loadVariant $ "variants/" ++ name ++ ".yaml"

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

-- | Same as above, but with less assurances.
mostSimilarMonsterHalfSane :: V.Variant -> T.Text -> Maybe T.Text
mostSimilarMonsterHalfSane variant text
    | T.length text >= 50 = Nothing
    | otherwise = let (_, result) = mostSimilarMonster variant text
                   in Just result


decideVariant :: [V.Variant] -> T.Text -> V.Variant
decideVariant variants name =
    fromMaybe
        (head variants) $
        find (\var -> V.commandPrefix var == name) variants

relevantFlag :: MD.MonsterFlag -> Maybe T.Text
relevantFlag MD.FlDisplaces = Just "displaces"
relevantFlag MD.FlVanDmgRduc = Just "damage reduction"
relevantFlag MD.FlBlinkAway = Just "blinks away"
relevantFlag MD.FlFairy = Just "fairy"
relevantFlag MD.FlScentTracker = Just "tracks scents"
relevantFlag MD.FlOviparous = Just "oviparous"
relevantFlag MD.FlTouchPetrifies = Just "touch petrifies"
relevantFlag MD.FlInvisible = Just "invisible"
relevantFlag MD.FlWallwalk = Just "phases"
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
relevantFlag MD.FlLithivore = Just "lithivore"
relevantFlag _ = Nothing

showB :: Show a => a -> TL.Builder
showB = TL.fromString . show

lineMonsterInformation :: MD.Monster -> T.Text
lineMonsterInformation mon = TL.toStrict $ TL.toLazyText $
    (TL.fromText $ MD.moName mon) <> " (" <> monsymbol <> ")" <>
    " | Lvl: " <> showB (MD.moBaseLevel mon) <>
    " | Diff: " <> showB (MD.moDifficulty mon) <>
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
    attackTypeName MD.AtIllurien = "illurien-swallow"
    attackTypeName MD.AtTinker = "tinker"
    attackTypeName MD.AtPhaseNonContact = "non-contacting-phase"
    attackTypeName MD.AtBeamNonContact = "non-contacting-beam"
    attackTypeName MD.AtMillionArms = "million-weaponized-arms"
    attackTypeName MD.AtSpin = "spin"

    attackDamageName MD.AdDimness = "dimness"
    attackDamageName MD.AdMapAmnesia = "map-amnesia"
    attackDamageName MD.AdIncreaseWeight = "increase-weight"
    attackDamageName MD.AdCast = "cast"
    attackDamageName MD.AdChaos = "chaos"
    attackDamageName MD.AdVomitInducing = "vomit-inducing"
    attackDamageName MD.AdNegativeEnchantment = "negative-enchantment"
    attackDamageName MD.AdVaporization = "vaporization"
    attackDamageName MD.AdStoneEdge = "stone-edge"
    attackDamageName MD.AdLitterBlob = "litter-blob"
    attackDamageName MD.AdCreateTrap = "create-trap"
    attackDamageName MD.AdRngIntervention = "rng-intervention"
    attackDamageName MD.AdIdentityAttack = "identity-attack"
    attackDamageName MD.AdFrenzy = "frenzy"
    attackDamageName MD.AdNether = "nether"
    attackDamageName MD.AdInsanity = "insanity"
    attackDamageName MD.AdNastyTrap = "nasty-trap"
    attackDamageName MD.AdSkillCapReduce = "skill-cap-reducting"
    attackDamageName MD.AdDreamAttack = "dream-eating"
    attackDamageName MD.AdBadRandomEffect = "bad-random-effect"
    attackDamageName MD.AdFumble = "fumble"
    attackDamageName MD.AdVenomous = "venomous"
    attackDamageName MD.AdVulnerability = "vulnerability-inducing"
    attackDamageName MD.AdCurseItems = "curse-items"
    attackDamageName MD.AdSludge = "sludge"
    attackDamageName MD.AdMasterBlaster = "masterblaster"
    attackDamageName MD.AdPits = "pits"
    attackDamageName MD.AdIceBlock = "iceblock"
    attackDamageName MD.AdStinkingCloud = "stinking-cloud"
    attackDamageName MD.AdFeelPain = "feel-pain"
    attackDamageName MD.AdDeadGaze = "deadly-gaze"
    attackDamageName MD.AdGravity = "gravity"
    attackDamageName MD.AdSound = "sound"
    attackDamageName MD.AdVampireDrain = "vampire-drain"
    attackDamageName MD.AdNegativeProtection = "negative-protection"
    attackDamageName MD.AdDepression = "depressing"
    attackDamageName MD.AdPoisonStat = "poison-sting"
    attackDamageName MD.AdNexus = "nexus"
    attackDamageName MD.AdSuckEquipment = "suck-equipment"
    attackDamageName MD.AdBanishment = "banishment"
    attackDamageName MD.AdCursedUnihorn = "cursed-unicorn-horn"
    attackDamageName MD.AdLazyness = "lazy"
    attackDamageName MD.AdPlasma = "plasma"
    attackDamageName MD.AdDrainsAllSortsOfStuff = "drains-all-sorts-of-stuff"
    attackDamageName MD.AdFakeMessages = "fake-message"
    attackDamageName MD.AdCharisma = "charisma-taking"
    attackDamageName MD.AdWrath = "wrath"
    attackDamageName MD.AdDrainLifeOrStats = "drain-life-and/or-stats"
    attackDamageName MD.AdInertia = "inertia"
    attackDamageName MD.AdThirsty = "thirsty"
    attackDamageName MD.AdMana = "mana"
    attackDamageName MD.AdSilverStarlightRapier = "silver-starlight-rapier"
    attackDamageName MD.AdRandomGaze = "random gaze"
    attackDamageName MD.AdHalfDragon = "half-dragon"
    attackDamageName MD.AdStealByTeleportation = "steal-by-teleportation"
    attackDamageName MD.AdFear = "fear"
    attackDamageName MD.AdBlackWebShadow = "black-web-shadow"
    attackDamageName MD.AdNetzach = "netzach"
    attackDamageName MD.AdWatcherTentacleGaze = "magical-tentacle-gaze"
    attackDamageName MD.AdNumb = "numb"
    attackDamageName MD.AdFreezeSolid = "freeze-solid"
    attackDamageName MD.AdWither = "wither"
    attackDamageName MD.AdBurn = "burn"
    attackDamageName MD.AdDisplacement = "displacement"
    attackDamageName MD.AdTinker = "tinker"
    attackDamageName MD.AdFireworks = "fireworks"
    attackDamageName MD.AdOona = "oona"
    attackDamageName MD.AdStudy = "study"
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
    attackDamageName MD.AdWebs = "webs"
    attackDamageName MD.AdWeeping = "levtele-drain"
    attackDamageName MD.AdGaro = "rumor-dispense"
    attackDamageName MD.AdGaroMaster = "oracle-dispense"
    attackDamageName MD.AdLoadstones = "loadstone-throw"
    attackDamageName MD.AdRemoveEngravings = "remove-engravings"
    attackDamageName MD.AdIllurien = "illurien-swallow"
    attackDamageName MD.AdLightRay = "light-ray"
    attackDamageName MD.AdRemoveLight = "remove-light"
    attackDamageName MD.AdDisarm = "disarm"

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
    monsymbol = "\x03" <> ircColor (MD.moColor mon) <> ",01" <>
                TLB.singleton (MD.moSymbol mon) <>
                "\x0f"
    ircColor :: MD.Color -> TL.Builder
    ircColor MD.Black = "14"
    ircColor MD.Red = "05"
    ircColor MD.Blue = "02"
    ircColor MD.BrightBlue = "12"
    ircColor MD.BrightMagenta = "13"
    ircColor MD.BrightCyan = "11"
    ircColor MD.Cyan = "10"
    ircColor MD.Orange = "04"
    ircColor MD.Green = "03"
    ircColor MD.Brown = "07"
    ircColor MD.Magenta = "06"
    ircColor MD.Gray = "15"
    ircColor MD.BrightGreen = "09"
    ircColor MD.White = "00"
    ircColor MD.Yellow = "08"

message :: IO (T.Text -> IO (Maybe T.Text))
message = do
    vars <- variants
    return $ messager vars
  where
    messager vars input
        | T.null input        = return Nothing
        | T.head input /= '@' = return Nothing
        | input == "@" = return Nothing
        | otherwise           = do
            case message' vars (T.tail input) of
              Right (Just ok) -> return (Just ok)
              Right Nothing -> return Nothing
              Left NoMonster -> return $ Just "No such monster."

filterNewLines :: String -> String
filterNewLines = fmap (\ch -> if ch == '\n' || ch == '\r' then ' ' else ch)

-- substandard parsec "string" function for texts
stringT :: Stream s m Char => T.Text -> ParsecT s u m T.Text
stringT txt = T.pack <$> string (T.unpack txt)

data NoMonster = NoMonster
  deriving ( Eq, Ord, Show, Read )

message' :: [V.Variant] -> T.Text -> Either NoMonster (Maybe T.Text)
message' variants input'
    | T.head input' == '@' = next (T.tail input') True
    | otherwise = next input' False
  where
    next input maximum_lev = case runParser (parser maximum_lev) () "line" input of
        Left errmsg -> Right $ Just $ T.pack $ filterNewLines $ show errmsg
        Right okay  -> okay

    parser :: Bool -> T.Parser (Either NoMonster (Maybe T.Text))
    parser maximum_lev = do
        (variantStr, ignore) <- foldl
                        (\previoustry variant ->
                          previoustry <|> try
                          ( (,) <$>
                           (stringT $ V.commandPrefix variant) <*>
                            try (stringT "?") ) )
                        (fail "") variants <|>
                        ((,) <$> try (stringT "")
                             <*> try (stringT "?")) <|>
                        (return ("", ""))
        if ignore == "?"
          then doPart maximum_lev $ decideVariant variants variantStr
          else return (Right Nothing)

    doPart maximum_lev variant = do
      spaces
      rawMonsterName <- many anyChar
      let monsterName = (T.strip . T.pack) rawMonsterName
      when (T.length monsterName <= 0) $
          fail $ ";I shall now launch the missiles that will cause " <>
                 "serious international side effects."
      return $ case msms variant monsterName of
          Nothing -> Left NoMonster
          Just mon -> Right $ Just $
              (if T.toLower monsterName /= T.toLower mon
                 then monsterName <> " ~" <>
                      (T.pack $ show $ distT monsterName mon) <> "~ "
                 else "") <>
              lineMonsterInformation
                  (fromJust $ V.monster variant mon)
      where
        msms = if maximum_lev then mostSimilarMonsterHalfSane else mostSimilarMonsterSane

