module NetHack.Data.Monster where

import qualified Data.Text as T
import qualified NetHack.Data.Dice as D

data Place = Dungeons | Gehennom | Sheol | Unique
             deriving (Eq, Show, Ord)

data AttackType = AtNone | AtClaw | AtBite | AtKick | AtButt |
              AtTouch | AtSting | AtHug | AtSpit |
              AtEngulf | AtBreath | AtExplode |
              AtSuicideExplode | AtGaze | AtTentacle |
              AtWeapon | AtCast | AtScre | AtMultiply |
              AtArrow | AtReach | AtMirror | AtWhip
              deriving (Eq, Show, Ord)

data DamageType = AdPhys | AdMagicMissile |
              AdFire | AdCold | AdSleep |
              AdDisintegrate | AdElectricity | AdStrDrain |
              AdAcid | AdSpecial1 | AdSpecial2 | AdBlind |
              AdStun | AdSlow | AdParalyse | AdLevelDrain |
              AdMagicDrain | AdLegs | AdStone | AdSticking |
              AdGoldSteal | AdItemSteal | AdSeduce |
              AdTeleport | AdRust | AdConfuse | AdDigest |
              AdHeal | AdWrap | AdWere | AdDexDrain |
              AdConDrain | AdIntDrain | AdDisease |
              AdRot | AdSex | AdHallucination | AdDeath |
              AdPestilence | AdFamine | AdSlime |
              AdDisenchant | AdCorrode | AdClerical |
              AdSpell | AdRandomBreath | AdAmuletSteal |
              AdCurse | AdLevelTeleport |
              AdBlink | AdFreeze | AdPunisher |
              AdDecapitate | AdBehead | AdCancellation |
              AdCalm | AdTickle | AdPoly | AdDrown |
              AdShred | AdJailer | AdBoulderArrow |
              AdBoulderArrowRandomSpread |
              AdMultiElementCounterAttackThatAngersTons |
              AdPoison | AdWisdom | AdVorpal | AdStealQuestArtifact |
              AdSpawnChaos | AdIronBall | AdGrow | AdSilver |
              AdAbduction | AdElementalGaze | AdUnknownPriest |
              AdLeviathan | AdAsmodeusBlood | AdMirror | AdMalk |
              AdTentacle | AdTele | AdHeadSpike | AdLethe | AdWet |
              AdHorn | AdSolar | AdEscalatingDamage | AdSoul | AdMist |
              AdSuck | AdDrainLuck | AdSpore | AdLava
              deriving (Eq, Show, Ord)

data MonsterSize = Tiny | Small | Medium |
                   Large | Huge | Gigantic deriving (Eq, Show, Ord)

data Color = Black | Red | Green | Brown | Blue | Magenta |
             Cyan | Gray | Orange | BrightGreen | Yellow |
             BrightBlue | BrightMagenta | BrightCyan | White
             deriving (Eq, Show, Ord)

data Resistance = ReFire | ReCold | ReSleep | ReDisintegrate |
              ReElectricity | RePoison | ReAcid |
              RePetrification |
              -- derived resistances below
              ReDrain | ReMagic
              deriving (Eq, Show, Ord)

data MonsterFlag = FlFly | FlSwim | FlAmorphous |
               FlWallwalk | FlCling | FlTunnel |
               FlNeedPick | FlConceal | FlHide |
               FlAmphibious | FlBreathless | FlNoTake |
               FlNoEyes | FlNoHands | FlNoLimbs |
               FlNoHead | FlMindless | FlHumanoid |
               FlAnimal | FlSlithy | FlUnSolid |
               FlThickHide | FlOviparous | FlRegen |
               FlSeeInvis | FlTeleport | FlTeleportControl |
               FlAcid | FlPoisonous | FlCarnivore |
               FlHerbivore | FlMetallivore |
               FlNoPoly | FlUndead | FlWere | FlHuman |
               FlElf | FlDwarf | FlGnome | FlOrc | FlDemon |
               FlMerc | FlLord | FlPrince | FlMinion |
               FlGiant | FlMale | FlFemale | FlNeuter |
               FlProperName | FlHostile | FlPeaceful |
               FlDomestic | FlWander | FlStalk |
               FlNasty | FlStrong | FlRockThrow |
               FlGreedy | FlJewels | FlCollect |
               FlMagicCollect | FlWantsAmulet |
               FlWantsBell | FlWantsBook | FlWantsCand |
               FlWantsArti | FlWantsAll | FlWaitsForYou |
               FlClose | FlCovetous | FlInfra |
               FlInfravisible |
               -- derived flags
               FlHatesSilver | FlPassesBars |
               FlVegan | FlVegetarian
               deriving (Eq, Show, Ord)

data Attack = Attack { atType :: AttackType,
                       atDamageType :: DamageType,
                       atDice :: D.Dice }
              deriving (Eq, Show, Ord)

data Monster = Monster { moName :: T.Text,
                         moSymbol :: Char,
                         moBaseLevel :: Int,
                         moSpeed :: Int,
                         moAC :: Int,
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
                         moColor :: Color }
               deriving(Eq, Show, Ord)

hasFlag :: MonsterFlag -> Monster -> Bool
hasFlag flag = elem flag . moFlags

