{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module NetHack.Data.Monster where

import Control.Applicative
import Control.Monad
import Data.Maybe
import qualified Data.Text as T
import Data.Yaml
import GHC.Generics
import qualified NetHack.Data.Dice as D

data Place = Dungeons | Gehennom | Sheol | Unique
  deriving (Eq, Show, Ord)

instance FromJSON Place where
  parseJSON (String "dungeons") = pure Dungeons
  parseJSON (String "gehennom") = pure Gehennom
  parseJSON (String "sheol") = pure Sheol
  parseJSON (String "unique") = pure Unique
  parseJSON _ = empty

data AttackType
  = AtNone
  | AtClaw
  | AtBite
  | AtKick
  | AtButt
  | AtTouch
  | AtSting
  | AtHug
  | AtSpit
  | AtEngulf
  | AtBreath
  | AtExplode
  | AtSuicideExplode
  | AtGaze
  | AtTentacle
  | AtWeapon
  | AtCast
  | AtScre
  | AtMultiply
  | AtArrow
  | AtReach
  | AtMirror
  | AtWhip
  | AtMMagical
  | AtReachingBite
  | AtLash
  | AtTrample
  | AtScratch
  | AtIllurien
  | AtTinker
  | AtPhaseNonContact
  | AtBeamNonContact
  | AtMillionArms
  | AtSpin
  | AtAny
  | AtRangedThorns
  | AtCloseRangeBreath
  | AtOffhandedWeapon
  | AtOffOffhandedWeapon
  | AtNonContactAttack
  | AtReachTouch
  | AtReachBite
  | AtPassiveWideGaze
  | AtHitsIfTwoPreviousHitsConnect
  | AtLashingVine
  | AtBlackGoat
  | AtAutoHit
  | AtAdjacent
  | AtTalk
  | AtTailSlap
  | AtVolley
  | AtWolfHeadBite
  deriving (Eq, Show, Ord, Generic)

instance FromJSON AttackType

data DamageType
  = AdPhys
  | AdMagicMissile
  | AdFire
  | AdCold
  | AdSleep
  | AdDisintegrate
  | AdElectricity
  | AdStrDrain
  | AdAcid
  | AdSpecial1
  | AdSpecial2
  | AdBlind
  | AdStun
  | AdSlow
  | AdParalyse
  | AdLevelDrain
  | AdMagicDrain
  | AdLegs
  | AdStone
  | AdSticking
  | AdGoldSteal
  | AdItemSteal
  | AdSeduce
  | AdTeleport
  | AdRust
  | AdConfuse
  | AdDigest
  | AdHeal
  | AdWrap
  | AdWere
  | AdDexDrain
  | AdConDrain
  | AdIntDrain
  | AdDisease
  | AdRot
  | AdSex
  | AdHallucination
  | AdDeath
  | AdPestilence
  | AdFamine
  | AdSlime
  | AdDisenchant
  | AdCorrode
  | AdClerical
  | AdSpell
  | AdRandomBreath
  | AdAmuletSteal
  | AdCurse
  | AdLevelTeleport
  | AdBlink
  | AdFreeze
  | AdPunisher
  | AdDecapitate
  | AdBehead
  | AdCancellation
  | AdCalm
  | AdTickle
  | AdPoly
  | AdDrown
  | AdShred
  | AdJailer
  | AdBoulderArrow
  | AdBoulderArrowRandomSpread
  | AdMultiElementCounterAttackThatAngersTons
  | AdPoison
  | AdWisdom
  | AdVorpal
  | AdStealQuestArtifact
  | AdSpawnChaos
  | AdIronBall
  | AdGrow
  | AdSilver
  | AdAbduction
  | AdElementalGaze
  | AdUnknownPriest
  | AdLeviathan
  | AdAsmodeusBlood
  | AdMirror
  | AdMalk
  | AdTentacle
  | AdTele
  | AdHeadSpike
  | AdLethe
  | AdWet
  | AdHorn
  | AdSolar
  | AdEscalatingDamage
  | AdSoul
  | AdMist
  | AdSuck
  | AdDrainLuck
  | AdSpore
  | AdLava
  | AdSunflower
  | AdFernExplosion
  | AdMandrake
  | AdPhysRetaliate
  | AdVamp
  | AdWebs
  | AdWeeping
  | AdGaro
  | AdGaroMaster
  | AdLoadstones
  | AdRemoveEngravings
  | AdIllurien
  | AdLightRay
  | AdRemoveLight
  | AdDisarm
  | AdStudy
  | AdOona
  | AdFireworks
  | AdTinker
  | AdNumb
  | AdFreezeSolid
  | AdDisplacement
  | AdWither
  | AdBurn
  | AdBlackWebShadow
  | AdNetzach
  | AdWatcherTentacleGaze
  | AdFear
  | AdStealByTeleportation
  | AdHalfDragon
  | AdRandomGaze
  | AdSilverStarlightRapier
  | AdPoisonStat
  | AdNexus
  | AdCursedUnihorn
  | AdSuckEquipment
  | AdGravity
  | AdPlasma
  | AdDrainsAllSortsOfStuff
  | AdSound
  | AdFakeMessages
  | AdVampireDrain
  | AdDepression
  | AdCharisma
  | AdNegativeProtection
  | AdLazyness
  | AdBanishment
  | AdWrath
  | AdDrainLifeOrStats
  | AdInertia
  | AdThirsty
  | AdMana
  | AdDeadGaze
  | AdFeelPain
  | AdStinkingCloud
  | AdPits
  | AdNastyTrap
  | AdSkillCapReduce
  | AdDreamAttack
  | AdBadRandomEffect
  | AdFumble
  | AdVenomous
  | AdVulnerability
  | AdCurseItems
  | AdSludge
  | AdMasterBlaster
  | AdDimness
  | AdMapAmnesia
  | AdIncreaseWeight
  | AdCast
  | AdChaos
  | AdVomitInducing
  | AdNegativeEnchantment
  | AdVaporization
  | AdStoneEdge
  | AdLitterBlob
  | AdCreateTrap
  | AdRngIntervention
  | AdIdentityAttack
  | AdIceBlock
  | AdFrenzy
  | AdNether
  | AdInsanity
  | AdIdentityNastiness
  | AdItemDamager
  | AdAntimatter
  | AdPain
  | AdTech
  | AdMemoryReduce
  | AdSkillReduce
  | AdStatDamage
  | AdGearDamage
  | AdThievery
  | AdLavaTiles
  | AdDeletesYourGame
  | AdDrainAlignment
  | AdAddSins
  | AdContamination
  | AdAggravateMonster
  | AdDestroyEq
  | AdTrembling
  | AdAny
  | AdCurseArmor
  | AdIncreaseSanity
  | AdReallyBadEffect
  | AdBleedout
  | AdShank
  | AdDrainScore
  | AdTerrainTerror
  | AdFeminism
  | AdLevitation
  | AdReduceMagicCancellation
  | AdIllusion
  | AdSpecificRegularAttack
  | AdSpecificNastyTrap
  | AdDebuff
  | AdNivellation
  | AdTechDrain
  | AdBlasphemy
  | AdDropItems
  | AdRemoveErosionProof
  | AdFlame
  | AdPsionic
  | AdLoud
  | AdKnockback
  | AdWater
  | AdDrainConstitution
  | AdPitAttack
  | AdImplantEgg
  | AdDrainStrength
  | AdDrainDexterity
  | AdDrainCharisma
  | AdFleshHook
  | AdMindWipe
  | AdSlowStoning
  | AdInflictDoubt
  | AdRevelatoryWhisper
  | AdPull
  | AdMercuryBlade
  | AdBloodFrenzy
  | AdFourSeasons
  | AdCreateSphere
  | AdElementalAcid
  | AdConflictTouch
  | AdElementalCold
  | AdElementalPoison
  | AdElementalFire
  | AdElementalElectric
  | AdArchonFire
  | AdDessicate
  | AdArrowOfSlaying
  | AdAntiBloodAttack
  | AdFirePoisonPhysicalBlindness
  | AdPollen
  | AdGoldify
  | AdMoonlightRapier
  | AdMummyRot
  | AdCharm
  | AdScald
  | AdEatGold
  | AdQuarkFlavour
  | AdMildHunger
  | AdShoe
  | AdLaser
  | AdNuke
  | AdUnholy
  | AdHoly
  | AdRock
  | AdLokoban
  | AdHalluSick
  | AdBigExplosion
  | AdYank
  | AdExplodingMMSpellbook
  | AdAlignmentBlast
  | AdReleaseAlignmentSpirits
  | AdCrystalMemories
  | AdDilithiumCrystals
  | AdMirrorBlast
  | AdVoidWhispers
  | AdWarMachineGaze
  | AdSimurgh
  | AdInjectLarva
  | AdMakeSkeletons
  | AdPotionEffect
  | AdKidnap
  | AdLaws
  | AdGetLost
  | AdTransmute
  | AdGrowHeads
  | AdForgetItems
  | AdWind
  | AdQuills
  | AdVoidDisintegrate
  | AdPerHitDie
  | AdSeverePoison
  | AdHolyUnholyEnergy
  deriving (Eq, Show, Ord, Generic)

instance FromJSON DamageType

instance ToJSON DamageType

data MonsterSize
  = Tiny
  | Small
  | Medium
  | Large
  | Huge
  | Gigantic
  deriving (Eq, Show, Ord, Generic)

instance FromJSON MonsterSize where
  parseJSON (String "tiny") = pure Tiny
  parseJSON (String "small") = pure Small
  parseJSON (String "medium") = pure Medium
  parseJSON (String "large") = pure Large
  parseJSON (String "huge") = pure Huge
  parseJSON (String "gigantic") = pure Gigantic
  parseJSON _ = empty

data Color
  = Black
  | Red
  | Green
  | Brown
  | Blue
  | Magenta
  | Cyan
  | Gray
  | Orange
  | BrightGreen
  | Yellow
  | BrightBlue
  | BrightMagenta
  | BrightCyan
  | White
  deriving (Eq, Show, Ord, Generic)

instance FromJSON Color

data Resistance
  = ReFire
  | ReCold
  | ReSleep
  | ReDisintegrate
  | ReElectricity
  | RePoison
  | ReAcid
  | RePetrification
  | -- derived resistances below
    ReDrain
  | ReMagic
  deriving (Eq, Show, Ord, Generic)

instance FromJSON Resistance

data MonsterFlag
  = FlFly
  | FlSwim
  | FlAmorphous
  | FlWallwalk
  | FlCling
  | FlTunnel
  | FlNeedPick
  | FlConceal
  | FlHide
  | FlAmphibious
  | FlBreathless
  | FlNoTake
  | FlNoEyes
  | FlNoHands
  | FlNoLimbs
  | FlNoHead
  | FlMindless
  | FlHumanoid
  | FlAnimal
  | FlSlithy
  | FlUnSolid
  | FlThickHide
  | FlOviparous
  | FlRegen
  | FlSeeInvis
  | FlTeleport
  | FlTeleportControl
  | FlAcid
  | FlPoisonous
  | FlCarnivore
  | FlHerbivore
  | FlMetallivore
  | FlNoPoly
  | FlUndead
  | FlWere
  | FlHuman
  | FlElf
  | FlDwarf
  | FlGnome
  | FlOrc
  | FlDemon
  | FlMerc
  | FlLord
  | FlPrince
  | FlMinion
  | FlGiant
  | FlMale
  | FlFemale
  | FlNeuter
  | FlProperName
  | FlHostile
  | FlPeaceful
  | FlDomestic
  | FlWander
  | FlStalk
  | FlNasty
  | FlStrong
  | FlRockThrow
  | FlGreedy
  | FlJewels
  | FlCollect
  | FlMagicCollect
  | FlWantsAmulet
  | FlWantsBell
  | FlWantsBook
  | FlWantsCand
  | FlWantsArti
  | FlWantsAll
  | FlWaitsForYou
  | FlClose
  | FlCovetous
  | FlInfra
  | FlInfravisible
  | FlInfravision
  | FlTraitor
  | FlUntameable
  | FlLithivore
  | FlPhasing
  | -- derived flags
    FlHatesSilver
  | FlPassesBars
  | FlVegan
  | FlVegetarian
  | FlPokemon
  | FlAvoider
  | FlTouchPetrifies
  | FlInvisible
  | FlScentTracker
  | FlFairy
  | FlBlinkAway
  | FlVanDmgRduc
  | FlDisplaces
  | FlClockwork
  | FlVampire
  deriving (Eq, Show, Ord, Generic)

instance FromJSON MonsterFlag

data Attack = Attack
  { atType :: AttackType,
    atDamageType :: DamageType,
    atDice :: D.Dice
  }
  deriving (Eq, Show, Ord, Generic)

instance FromJSON Attack where
  parseJSON (Array [val1, val2, val3, val4]) = do
    atype <- parseJSON val1
    dtype <- parseJSON val2
    dice_top <- parseJSON val3
    dice_bottom <- parseJSON val4
    return $
      Attack
        { atType = atype,
          atDamageType = dtype,
          atDice = D.Dice dice_top dice_bottom
        }
  parseJSON _ = empty

data Monster = Monster
  { moName :: T.Text,
    moSymbol :: Char,
    moBaseLevel :: Int,
    moDifficulty :: Int,
    moSpeed :: Int,
    moAC :: Either Int T.Text,
    moMR :: Int,
    moAlign :: Int,
    moGenerationPlaces :: [Place],
    moLeavesCorpse :: Bool,
    moNotGeneratedNormally :: Bool,
    moAppearsInSmallGroups :: Bool,
    moAppearsInLargeGroups :: Bool,
    moGenocidable :: Bool,
    moAttacks :: [Attack],
    moWeight :: Int,
    moNutrition :: Int,
    moSize :: MonsterSize,
    moResistances :: [Resistance],
    moConferred :: [Resistance],
    moFlags :: [MonsterFlag],
    moColor :: Color
  }
  deriving (Eq, Show, Ord, Generic)

hasFlag :: MonsterFlag -> Monster -> Bool
hasFlag flag = elem flag . moFlags

instance FromJSON Monster where
  parseJSON (Object v) = do
    name <- v .: "name"
    symb <- v .: "symbol"
    when (symb == "") $ fail "Must have a symbol."
    baselevel <- v .: "base-level"
    speed <- v .: "speed"
    ac_int_maybe <- (Just <$> v .: "ac") <|> pure Nothing :: Parser (Maybe Int)
    ac_str_maybe <- (Just <$> v .: "ac") <|> pure Nothing :: Parser (Maybe T.Text)
    ac <- case (ac_int_maybe, ac_str_maybe) of
      (Nothing, Nothing) ->
        fail $ "Cannot parse AC as either int or str"
      (Just ac_int, _) -> return $ Left ac_int
      (_, Just ac_str) -> return $ Right ac_str
    mr <- v .: "mr"
    diff <- fromMaybe 0 <$> (v .:? "difficulty")
    align <- v .: "alignment"
    generates <- v .: "generates" <|> pure []
    corpse <- v .: "leaves-corpse"
    notnormallygenerated <- v .: "not-generated-normally"
    smallgroups <- v .: "appears-in-small-groups"
    largegroups <- v .: "appears-in-large-groups"
    genocidable <- v .: "genocidable"
    attacks <- v .: "attacks"
    weight <- v .: "weight"
    nutr <- v .: "nutrition"
    size <- v .: "size"
    resis <- v .: "resistances" <|> pure []
    confers <- v .: "conferred" <|> pure []
    flags <- v .: "flags"
    color <- v .: "color"
    return
      Monster
        { moName = name,
          moSymbol = T.head symb,
          moDifficulty = diff,
          moBaseLevel = baselevel,
          moSpeed = speed,
          moAC = ac,
          moAlign = align,
          moMR = mr,
          moGenerationPlaces = generates,
          moLeavesCorpse = corpse,
          moNotGeneratedNormally = notnormallygenerated,
          moAppearsInSmallGroups = smallgroups,
          moAppearsInLargeGroups = largegroups,
          moGenocidable = genocidable,
          moAttacks = attacks,
          moSize = size,
          moNutrition = nutr,
          moWeight = weight,
          moResistances = resis,
          moConferred = confers,
          moFlags = flags,
          moColor = color
        }
  parseJSON _ = empty
