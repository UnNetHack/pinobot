{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Bot
  ( message,
  )
where

import Control.Applicative hiding
  ( many,
    (<|>),
  )
import Control.Monad hiding (mapM_)
import Control.Monad.Trans.Writer
import Data.List ( nub )
import Data.Foldable
import Data.Map.Strict ( Map )
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Monoid
import Data.Set ( Set )
import qualified Data.Set as S
import Data.String
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TL
import qualified Data.Text.Lazy.Builder as TLB
import IRC.ConfigFile
import NetHack.Data.Dice
import qualified NetHack.Data.Monster as MD
import qualified NetHack.Data.Variant as V
import Text.Parsec
import qualified Text.Parsec.Text as T
import System.IO
import Prelude hiding
  ( concatMap,
    foldl,
    foldl1,
    mapM_,
  )

variantNames :: [String]
variantNames =
  [
    "Vanilla", -- First one is used by default,
               -- if you query PinoBot with just @? in IRC.
    "Vanilla343",
    "UnNetHack",
    "UnNetHackPlus",
    "SporkHack",
    "GruntHack",
    "Slashem",
    "Brass",
    "Dnethack",
    "Notdnethack",
    "Notnotdnethack",
    "SlashemExtended",
    "SlashTHEM",
    "Fourk",
    "EvilHack",
    "XNetHack",
    "SpliceHack",
    "Hackem"
  ]

-- Uses configuration to filter variants.
-- Returns triple: (enabled, disabled, unknown)
-- where unknown is the variants found in config, that are
-- unrecognized by variantNames above.
filterByConf :: Configuration -> [String] -> ([String], [String], [String])
filterByConf conf all_variants =
  (filter (flip S.notMember disabled_variants) all_variants
  ,filter (flip S.member disabled_variants) all_variants
  ,filter (flip S.notMember variant_names) (S.toList disabled_variants))
 where
  variant_names :: Set String
  variant_names = S.fromList variantNames

  disabled_variants :: Set String
  disabled_variants = S.fromList $ fmap T.unpack $ disabledVariants conf

-- Sometimes refers to Nth row or Nth column by character. Sometimes refers to
-- Nth *table* row or Nth *table* column. Who needs consistency. Also not used
-- at all sometimes (you see Int instead).
type Row = Int
type Col = Int

-- | Used in renderPompousTable. HorizontalLine on any column in a row triggers
-- creation of a horizontal line and not rendering the row otherwise.
data TableCell = OutOfRange | Text !T.Text | HorizontalLine
  deriving ( Eq, Ord, Show, Read )

text :: String -> TableCell
text str = Text $ T.pack str

instance IsString TableCell where
  fromString str = Text (T.pack str)

cellText :: TableCell -> T.Text
cellText (Text txt) = txt
cellText _ = ""

cellLength :: TableCell -> Int
cellLength cell = T.length $ cellText cell

-- | Render a ridiculously pompous table and return it as a string (that could
-- then be printed to stdout).
--
-- Automatically sizes columns.
renderPompousTable :: (Row -> Col -> TableCell) -- ^ Get text to be in a column.
                                                --   Return OutOfRange if out of range
                                                --   (critical for this
                                                --   function to figure out the
                                                --   size of the table). If a
                                                --   cell exists but doesn't
                                                --   have anything, return Text
                                                --   "" instead. This function
                                                --   scans by starting from (0,
                                                --   0) (top-left) and going
                                                --   line by line until a
                                                --   Nothing is returned for
                                                --   (0, y) for some y.
                                                --
                                                --   Zeroth row is considered
                                                --   header.
                   -> T.Text
renderPompousTable get_cell_content = runTextBuilder $ unless (M.null cell_contents) $ do
  tellHorizontalLine
  -- Header
  tellCells 0
  tellHorizontalLine
  -- Cells
  for_ [1..n_cells_height-1] $ \row_idx ->
    tellCells row_idx
  tellHorizontalLine
 where
  -- Runs a writer monad on TextBuilder, returns a strict text in the end.
  runTextBuilder :: Writer TL.Builder () -> T.Text
  runTextBuilder action =
    let final_builder = execWriter action
     in TL.toStrict $ TL.toLazyText final_builder

  tellHorizontalLine :: Writer TL.Builder ()
  tellHorizontalLine = do
    tell "+"
    for_ [0..total_width - 2 - 1] $ \nth_dash -> do
      let col = nth_dash + 1
      if col `S.member` starry_columns
        then tell "+"
        else tell "-"
    tell "+\n"

  -- Renders one row, putting pipes in the right places and respecting column
  -- widths.
  tellCells :: Int -> Writer TL.Builder ()
  tellCells row = do
    if row `S.member` horizontal_lines
      then tellHorizontalLine
      else tellCellsLikeActualTextCellAndNotAPompousLine row

  tellCellsLikeActualTextCellAndNotAPompousLine :: Int -> Writer TL.Builder ()
  tellCellsLikeActualTextCellAndNotAPompousLine row = do
    for_ [0..n_cells_width-1] $ \column_idx -> do
      tell "|"
      let text = fromMaybe "" $ fmap cellText $ M.lookup (column_idx, row) cell_contents
          column_size = fromMaybe 0 $ M.lookup column_idx column_sizes
          text_len = T.length text
      tell " "
      tell $ TL.fromText text
      tell $ TL.fromText $ T.replicate (column_size - text_len - 1) " "

    tell "|\n"

  n_cells_width :: Int
  n_cells_width = if M.null cell_contents then 0 else
    (maximum $ fmap fst $ M.keys cell_contents) + 1

  n_cells_height :: Int
  n_cells_height = if M.null cell_contents then 0 else
    (maximum $ fmap snd $ M.keys cell_contents) + 1

  -- set of rows that should be horizontal lines
  horizontal_lines :: Set Row
  horizontal_lines = S.fromList $
    fmap snd $
    M.keys $
    M.filter (\cell -> cell == HorizontalLine) cell_contents

  -- Set of columns where we should put + instead of - in any horizontal line.
  -- Important for the pompousness part of this function.
  --
  -- Should agree with the renderer part.
  starry_columns :: Set Col
  starry_columns = go 0 1 (S.singleton 0)
   where
    go :: Int -> Col -> Set Col -> Set Col
    go column_idx !cursor !set | column_idx < n_cells_width =
      let cell_width = fromMaybe 0 $ M.lookup column_idx column_sizes
          new_cursor = cursor + cell_width + 1 -- the + 1 for | between table columns
       in go (column_idx+1) new_cursor (S.insert (new_cursor-1) set)
    go _ _ set = set

  -- column_sizes: a map that tells column widths. This counts the number of
  -- columns inside the cell (not counting cell borders).
  --
  -- key: column
  -- value: how wide the column should be (max length of longest text + 2 for
  -- padding)
  column_sizes :: Map Int Int
  column_sizes = if n_cells_width == 0 then mempty else go 0 mempty
   where
    go :: Int -> Map Int Int -> Map Int Int
    go column !accum | column < n_cells_width =
      let cells :: [TableCell]
          cells = M.elems $ M.filterWithKey (\(x, _y) _ -> x == column) cell_contents

          column_width :: Int
          column_width = if null cells then 0 else maximum (fmap cellLength cells) + 2
       in go (column+1) (M.insert column column_width accum)
    go _ accum = accum

  -- total width of the table. this accounts for all decorations.
  --
  -- +-----+-----+
  -- |  x  |  y  |
  -- +-----+-----+
  -- ^           ^
  -- |           |
  -- + - - + - - +
  --       |
  --       +- -  "total_width"
  --
  -- cell widths (column_size) + 1 + n_cells_width
  total_width = if M.null column_sizes then 0 else
    (sum (M.elems column_sizes) + 1 + n_cells_width)

  cell_contents :: Map (Col, Row) TableCell
  cell_contents = go 0 0 mempty
   where
    go :: Col -> Row -> Map (Col, Row) TableCell -> Map (Col, Row) TableCell
    go !x !y !accum = case get_cell_content y x of
      OutOfRange | x > 0 -> go 0 (y+1) accum
      OutOfRange | x == 0 -> accum
      Text txt -> go (x+1) y (M.insert (x, y) (Text txt) accum)
      HorizontalLine -> go (x+1) y (M.insert (x, y) HorizontalLine accum)

-- Load up variant YAML files, and uses the Pinobot config file to decide
-- which ones to load.
--
-- Prints to stdout what it loaded, and what it did not load (due to disabling
-- in pinobot_config.toml)
variants :: Configuration -> IO [V.Variant]
variants conf = do
  let (enabled_variant_names, disabled_variant_names, unknown_variant_names) =
        filterByConf conf variantNames

      n_enabled_variants = length enabled_variant_names
      n_disabled_variants = length disabled_variant_names
      n_unknown_variants = length unknown_variant_names

  loaded_variants <- sequence $ variantify enabled_variant_names

  let -- ranges for the pompous table, which parts list which variants.
      -- [start, end] (i.e. end is inclusive)
      enabled_variants_start = 1 -- 0 is header, so first row is 1
      enabled_variants_end = enabled_variants_start + n_enabled_variants - 1

      disabled_variants_start = enabled_variants_end + 2  -- account for horizontal line
      disabled_variants_end = disabled_variants_start + n_disabled_variants - 1

      unknown_variants_start = disabled_variants_end + 2
      unknown_variants_end = unknown_variants_start + n_unknown_variants - 1

      horizontal_lines = S.fromList $
        (if disabled_variants_end >= disabled_variants_start
           then [enabled_variants_end + 1]
           else []) <>
        (if unknown_variants_end >= unknown_variants_start
          then [disabled_variants_end+1]
          else [])

      -- TODO: reduce repetition
      nmonsters_by_variant_name :: T.Text -> TableCell
      nmonsters_by_variant_name name = case find (\variant -> T.toLower (V.variantName variant) == T.toLower name) loaded_variants of
        Nothing -> "?"
        Just variant -> text $ show $ V.numMonsters variant

      source_by_variant_name :: T.Text -> TableCell
      source_by_variant_name name = case find (\variant -> T.toLower (V.variantName variant) == T.toLower name) loaded_variants of
        Nothing -> ""
        Just variant -> Text $ fromMaybe "" $ V.source variant

      last_updated_by_variant_name :: T.Text -> TableCell
      last_updated_by_variant_name name = case find (\variant -> T.toLower (V.variantName variant) == T.toLower name) loaded_variants of
        Nothing -> ""
        Just variant -> Text $ fromMaybe "" $ V.lastUpdated variant

      pompous_table = renderPompousTable $ \row column -> case (row, column) of
        (_, col) | col >= 5 -> OutOfRange
        (row, _) | row `S.member` horizontal_lines -> HorizontalLine
        (0, 0) -> "Variant"
        (0, 1) -> "Status"
        (0, 2) -> "#Monsters"
        (0, 3) -> "Updated"
        (0, 4) -> "Source"
        (0, _) -> OutOfRange
        -- enabled variants
        (row, _) | row >= enabled_variants_start && row <= enabled_variants_end ->
          let variant_idx = row - enabled_variants_start
              variant_name = enabled_variant_names !! variant_idx
           in case column of
                0 -> text variant_name
                1 -> ""
                2 -> nmonsters_by_variant_name (T.pack variant_name)
                3 -> last_updated_by_variant_name (T.pack variant_name)
                4 -> source_by_variant_name (T.pack variant_name)
                _ -> OutOfRange
        -- disabled variants
        (row, _) | row >= disabled_variants_start && row <= disabled_variants_end ->
          case column of
            0 -> text $ disabled_variant_names !! (row - disabled_variants_start)
            1 -> "DISABLED"
            2 -> "n/a"
            _ -> OutOfRange
        -- unknown variants
        (row, _) | row >= unknown_variants_start && row <= unknown_variants_end ->
          case column of
            0 -> text $ unknown_variant_names !! (row - unknown_variants_start)
            1 -> "UNKNOWN"
            2 -> "n/a"
            _ -> OutOfRange
        _ -> OutOfRange

  unless (T.null pompous_table) $ do
    T.putStrLn ""
    T.putStrLn pompous_table
    hFlush stdout

  unless (null unknown_variant_names) $
    putStrLn $ "WARNING: Unknown variant names were found in the configuration and they will be ignored: " <> show unknown_variant_names

  pure loaded_variants
 where
  variantify = fmap $ \name -> V.loadVariant $ "variants/" ++ name ++ ".yaml"

-- Shamelessly stolen from HaskellWiki which in turn stole it from Lloyd
-- Allison:
-- http://www.csse.monash.edu.au/~lloyd/tildeStrings/Alignment/92.IPL.html
--
-- Calculates the Levenshtein distance between two lists.
dist :: Eq a => [a] -> [a] -> Int
dist a b =
  last
    ( if lab == 0
        then mainDiag
        else
          if lab > 0
            then lowers !! (lab - 1)
            else {- < 0 -}
              uppers !! (-1 - lab)
    )
  where
    mainDiag = oneDiag a b (head uppers) (-1 : head lowers)
    uppers = eachDiag a b (mainDiag : uppers) -- upper diagonals
    lowers = eachDiag b a (mainDiag : lowers) -- lower diagonals
    eachDiag _ [] _ = []
    eachDiag a (_ : bs) (lastDiag : diags) =
      oneDiag a bs nextDiag lastDiag : eachDiag a bs diags
      where
        nextDiag = head (tail diags)
    eachDiag _ _ _ = undefined -- suppresses warnings
    oneDiag a b diagAbove diagBelow = thisdiag
      where
        doDiag [] _ _ _ _ = []
        doDiag _ [] _ _ _ = []
        doDiag (ach : as) (bch : bs) nw n w =
          me
            : doDiag as bs me (tail n) (tail w)
          where
            me = if ach == bch then nw else 1 + min3 (head w) nw (head n)
        firstelt = 1 + head diagBelow
        thisdiag = firstelt : doDiag a b firstelt diagAbove (tail diagBelow)
    lab = length a - length b
    min3 x y z = if x < y then x else min y z

distT :: T.Text -> T.Text -> Int
distT t1 t2 = dist (T.unpack $ T.toLower t1) (T.unpack $ T.toLower t2)

-- Returns the most similar monster name along with its levenshtein distance.
mostSimilarMonster :: V.Variant -> T.Text -> (Int, T.Text)
mostSimilarMonster variant name =
  minimumBy (\(a1, _) (a2, _) -> a1 `compare` a2) $
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
  | T.length text >= 60 =
      Nothing
  | otherwise =
      let (distance, result) = mostSimilarMonster variant text
       in if distance <= 3 then Just result else Nothing

-- | Same as above, but with less assurances.
mostSimilarMonsterHalfSane :: V.Variant -> T.Text -> Maybe T.Text
mostSimilarMonsterHalfSane variant text
  | T.length text >= 60 =
      Nothing
  | otherwise =
      let (_, result) = mostSimilarMonster variant text in Just result

decideVariant :: [V.Variant] -> T.Text -> V.Variant
decideVariant variants name =
  fromMaybe (head variants) $
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
relevantFlag MD.FlRegen = Just "regen"
relevantFlag MD.FlUnSolid = Just "unsolid"
relevantFlag MD.FlInfravisible = Just "infravis"
relevantFlag MD.FlCovetous = Just "covetous"
relevantFlag MD.FlMindless = Just "mindless"
relevantFlag MD.FlNoPoly = Just "nopoly"
relevantFlag MD.FlTeleport = Just "tele"
relevantFlag MD.FlUndead = Just "undead"
relevantFlag MD.FlDemon = Just "demon"
relevantFlag MD.FlVegan = Just "vegan"
relevantFlag MD.FlVegetarian = Just "vegetarian"
relevantFlag MD.FlStalk = Just "stalker"
relevantFlag MD.FlMetallivore = Just "metallivore"
relevantFlag MD.FlPoisonous = Just "poisonous"
relevantFlag MD.FlLithivore = Just "lithivore"
relevantFlag MD.FlPassesBars = Just "passes-bars"
relevantFlag MD.FlHatesSilver = Just "silverhate"
relevantFlag MD.FlDruidForm = Just "druid form"
relevantFlag _ = Nothing

-- Converts any druid-form flag to just FlDruidForm. E.g. FlDruidFormA ->
-- FlDruidForm.
collapseDruidForm :: MD.MonsterFlag -> MD.MonsterFlag
collapseDruidForm MD.FlDruidForm = MD.FlDruidForm
collapseDruidForm MD.FlDruidFormA = MD.FlDruidForm
collapseDruidForm MD.FlDruidFormB = MD.FlDruidForm
collapseDruidForm MD.FlDruidFormC = MD.FlDruidForm
collapseDruidForm MD.FlDruidFormD = MD.FlDruidForm
collapseDruidForm other = other

showB :: Show a => a -> TL.Builder
showB = TL.fromString . show

lineMonsterInformation :: MD.Monster -> T.Text
lineMonsterInformation mon =
  TL.toStrict $
    TL.toLazyText $
      (TL.fromText $ MD.moName mon)
        <> " ("
        <> monsymbol
        <> ")"
        <> " | Lvl: "
        <> showB (MD.moBaseLevel mon)
        <> " | Diff: "
        <> showB (MD.moDifficulty mon)
        <> " | Spd: "
        <> showB (MD.moSpeed mon)
        <> " | Res: "
        <> resistances (MD.moResistances mon)
        <> "| Confer: "
        <> confers (MD.moConferred mon)
        <> "| MR: "
        <> showB (MD.moMR mon)
        <> " | Gen: "
        <> generates (MD.moGenerationPlaces mon)
        <> "| AC: "
        <> case MD.moAC mon of
          Left ac_int -> showB ac_int
          Right ac_str -> TL.fromText ac_str
        <> " | Atk: "
        <> attacks (MD.moAttacks mon)
        <> " | Align: "
        <> showB (MD.moAlign mon)
        <> " | "
        <> flags
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
        else do
          when isCarnivore $ tell ["carnivore"]
          when isHerbivore $ tell ["herbivore"]
      mapM_ (tell . fmap TL.fromText . catMaybes . return . relevantFlag) $
        nub $ fmap collapseDruidForm $ MD.moFlags mon

    relevantFlags :: [TL.Builder]
    relevantFlags = execWriter relevantFlagsW

    flags :: TL.Builder
    flags
      | null relevantFlags = "none"
      | null $ tail relevantFlags = head relevantFlags
      | otherwise = foldl1 (\accum str -> accum <> ", " <> str) relevantFlags

    attacks :: [MD.Attack] -> TL.Builder
    attacks [] = "none"
    attacks [attack] = attackName attack
    attacks xs = foldl1 (\accum str -> accum <> ", " <> str) (fmap attackName xs)

    attackName :: MD.Attack -> TL.Builder
    attackName (MD.Attack atype dtype (Dice d1 d2)) =
      showB d1
        <> "d"
        <> showB d2
        <> " "
        <> attackTypeName atype
        <> " "
        <> attackDamageName dtype

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
    attackTypeName MD.AtMMagical = "monster-only-magic"
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
    attackTypeName MD.AtAny = "any"
    attackTypeName MD.AtRangedThorns = "ranged-thorns"
    attackTypeName MD.AtCloseRangeBreath = "close-range-breath"
    attackTypeName MD.AtOffhandedWeapon = "offhanded-weapon"
    attackTypeName MD.AtOffOffhandedWeapon = "offoffhanded-weapon"
    attackTypeName MD.AtNonContactAttack = "non-contact-attack"
    attackTypeName MD.AtReachTouch = "longreaching-touch"
    attackTypeName MD.AtReachBite = "longreaching-bite"
    attackTypeName MD.AtPassiveWideGaze = "passive-gaze"
    attackTypeName MD.AtHitsIfTwoPreviousHitsConnect = "hits-if-two-previous-hits-connect"
    attackTypeName MD.AtLashingVine = "lashing-vines"
    attackTypeName MD.AtBlackGoat = "black-goat-shenanigans"
    attackTypeName MD.AtAutoHit = "autohit"
    attackTypeName MD.AtAdjacent = "adjacent"
    attackTypeName MD.AtTalk = "talk"
    attackTypeName MD.AtTailSlap = "tailslap"
    attackTypeName MD.AtVolley = "volley"
    attackTypeName MD.AtWolfHeadBite = "wolfhead-bite"
    attackTypeName MD.AtTongue = "tongue"
    attackTypeName MD.AtVomit = "vomit"

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
      "multi-elemental-counterattack-that-angers-tons" -- wtf
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
    attackDamageName MD.AdIdentityNastiness = "identity-nastiness"
    attackDamageName MD.AdItemDamager = "item-damaging"
    attackDamageName MD.AdAntimatter = "antimatter"
    attackDamageName MD.AdPain = "PAIN"
    attackDamageName MD.AdTech = "technology"
    attackDamageName MD.AdMemoryReduce = "memory-reduce"
    attackDamageName MD.AdSkillReduce = "skill-reduce"
    attackDamageName MD.AdStatDamage = "stat-damage"
    attackDamageName MD.AdGearDamage = "gear-damaging"
    attackDamageName MD.AdThievery = "thievery"
    attackDamageName MD.AdLavaTiles = "lava-tiles"
    attackDamageName MD.AdDeletesYourGame = "data-delete"
    attackDamageName MD.AdDrainAlignment = "drain-alignment"
    attackDamageName MD.AdAddSins = "makes-you-a-dirty-sinner"
    attackDamageName MD.AdContamination = "contamination"
    attackDamageName MD.AdAggravateMonster = "makes-you-aggravate-monsters"
    attackDamageName MD.AdDestroyEq = "destroys-equipment"
    attackDamageName MD.AdTrembling = "gives-you-parkinsons"
    attackDamageName MD.AdAny = "any"
    attackDamageName MD.AdCurseArmor = "curse-armor"
    attackDamageName MD.AdIncreaseSanity = "increase-sanity"
    attackDamageName MD.AdReallyBadEffect = "really-bad-effect"
    attackDamageName MD.AdBleedout = "bleedout"
    attackDamageName MD.AdShank = "shank"
    attackDamageName MD.AdDrainScore = "drain-score"
    attackDamageName MD.AdTerrainTerror = "terrain-terror"
    attackDamageName MD.AdFeminism = "feminism"
    attackDamageName MD.AdLevitation = "levitation"
    attackDamageName MD.AdReduceMagicCancellation = "reduce-magic-cancellation"
    attackDamageName MD.AdIllusion = "illusion"
    attackDamageName MD.AdSpecificRegularAttack = "specific-regular-attack"
    attackDamageName MD.AdSpecificNastyTrap = "specific-nasty-trap"
    attackDamageName MD.AdDebuff = "debuff"
    attackDamageName MD.AdNivellation = "nivellation"
    attackDamageName MD.AdTechDrain = "technique-drain"
    attackDamageName MD.AdBlasphemy = "makes-your-god-angry-at-you"
    attackDamageName MD.AdDropItems = "drop-items"
    attackDamageName MD.AdRemoveErosionProof = "remove-erosion-proof"
    attackDamageName MD.AdFlame = "flame"
    attackDamageName MD.AdPsionic = "psionic"
    attackDamageName MD.AdLoud = "loud"
    attackDamageName MD.AdKnockback = "knockback"
    attackDamageName MD.AdWater = "water"
    attackDamageName MD.AdPitAttack = "create-pit"
    attackDamageName MD.AdDrainConstitution = "drain-constitution"
    attackDamageName MD.AdDrainStrength = "drain-strength"
    attackDamageName MD.AdDrainCharisma = "drain-charisma"
    attackDamageName MD.AdDrainDexterity = "drain-dexterity"
    attackDamageName MD.AdFleshHook = "flesh-hook"
    attackDamageName MD.AdImplantEgg = "implant-egg"
    attackDamageName MD.AdDessicate = "dessicate"
    attackDamageName MD.AdArrowOfSlaying = "arrow-of-slaying"
    attackDamageName MD.AdArchonFire = "archon-fire"
    attackDamageName MD.AdGoldify = "goldify"
    attackDamageName MD.AdMoonlightRapier = "moonlight-rapier"
    attackDamageName MD.AdMummyRot = "rot"
    attackDamageName MD.AdMindWipe = "mindwipe"
    attackDamageName MD.AdSlowStoning = "slow-stoning"
    attackDamageName MD.AdInflictDoubt = "inflict-doubt"
    attackDamageName MD.AdRevelatoryWhisper = "revelatory-whisper"
    attackDamageName MD.AdPull = "pull"
    attackDamageName MD.AdMercuryBlade = "mercury-blade"
    attackDamageName MD.AdBloodFrenzy = "bloodfrenzy"
    attackDamageName MD.AdPollen = "pollen"
    attackDamageName MD.AdElementalCold = "elemental-cold"
    attackDamageName MD.AdElementalPoison = "elemental-poison"
    attackDamageName MD.AdElementalFire = "elemental-fire"
    attackDamageName MD.AdElementalElectric = "elemental-electric"
    attackDamageName MD.AdElementalAcid = "elemental-acid"
    attackDamageName MD.AdFourSeasons = "fourseasons"
    attackDamageName MD.AdCreateSphere = "create-sphere"
    attackDamageName MD.AdConflictTouch = "conflict-touch"
    attackDamageName MD.AdAntiBloodAttack = "antiblood-attack"
    attackDamageName MD.AdFirePoisonPhysicalBlindness = "fire+poison+physical+blind"
    attackDamageName MD.AdCharm = "charm"
    attackDamageName MD.AdScald = "scald"
    attackDamageName MD.AdEatGold = "eat-gold"
    attackDamageName MD.AdQuarkFlavour = "quark-flavour"
    attackDamageName MD.AdMildHunger = "mild-hunger"
    attackDamageName MD.AdShoe = "SHOE-ATTACK"
    attackDamageName MD.AdLaser = "laser"
    attackDamageName MD.AdNuke = "nuke"
    attackDamageName MD.AdUnholy = "unholy"
    attackDamageName MD.AdHoly = "holy"
    attackDamageName MD.AdLokoban = "lokoban"
    attackDamageName MD.AdRock = "rock"
    attackDamageName MD.AdHalluSick = "hallusick"
    attackDamageName MD.AdYank = "yank"
    attackDamageName MD.AdBigExplosion = "big-explosion"
    attackDamageName MD.AdExplodingMMSpellbook = "exploding-magic-missile-spellbooks"
    attackDamageName MD.AdAlignmentBlast = "alignment-blast"
    attackDamageName MD.AdReleaseAlignmentSpirits = "release-alignment-spirits"
    attackDamageName MD.AdCrystalMemories = "crystal-memories"
    attackDamageName MD.AdDilithiumCrystals = "dilithium-crystals"
    attackDamageName MD.AdMirrorBlast = "mirror-blast"
    attackDamageName MD.AdVoidWhispers = "void-whispers"
    attackDamageName MD.AdWarMachineGaze = "war-machine-gaze"
    attackDamageName MD.AdSimurgh = "simurgh"
    attackDamageName MD.AdInjectLarva = "inject-larva"
    attackDamageName MD.AdMakeSkeletons = "make-skeletons"
    attackDamageName MD.AdPotionEffect = "potion-effect"
    attackDamageName MD.AdKidnap = "kidnap"
    attackDamageName MD.AdLaws = "law"
    attackDamageName MD.AdGetLost = "get-lost"
    attackDamageName MD.AdTransmute = "transmute"
    attackDamageName MD.AdGrowHeads = "grow-heads"
    attackDamageName MD.AdForgetItems = "1%-forget-items"
    attackDamageName MD.AdWind = "wind"
    attackDamageName MD.AdQuills = "quills"
    attackDamageName MD.AdVoidDisintegrate = "void-disintegrate"
    attackDamageName MD.AdPerHitDie = "per-hit-die"
    attackDamageName MD.AdSeverePoison = "severe-poison"
    attackDamageName MD.AdHolyUnholyEnergy = "holy-unholy-energy"
    attackDamageName MD.AdByakheeEggs = "byakhee-eggs"
    attackDamageName MD.AdTentaclesStealMagicItems = "tentacle-magic-item-steal"
    attackDamageName MD.AdBlackStarRapier = "black-star-rapier"
    attackDamageName MD.AdPiercingScream = "piercing-stream"
    attackDamageName MD.AdSong = "song"
    attackDamageName MD.AdGibber = "gibber"
    attackDamageName MD.AdUnnervingGaze = "unnerving-gaze"
    attackDamageName MD.AdMadnessFire = "madness-fire"
    attackDamageName MD.AdForcesTargetToAttack = "forces-target-to-attack"
    attackDamageName MD.AdDrainsBonusHp = "drains-bonus-hp"
    attackDamageName MD.AdPush = "push"
    attackDamageName MD.AdLick = "lick"
    attackDamageName MD.AdPoisonRot = "poison-rot"
    attackDamageName MD.AdAcidStabBone = "acidstabbone"

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
    monsymbol =
      "\x03"
        <> ircColor (MD.moColor mon)
        <> ",01"
        <> TLB.singleton (MD.moSymbol mon)
        <> "\x0f"
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

message :: Configuration -> IO (T.Text -> IO (Maybe T.Text))
message conf = do
  vars <- variants conf
  return $ messager vars
  where
    -- This code matches @? or @x? (where x is variant) anywhere on an IRC line.
    -- This makes it possible for people who are using "IRC connectors" (like the
    -- one that makes Discord users appear on IRC through an IRC bot) to use
    -- Pinobot.
    --
    -- E.g.
    --
    -- @v?test
    --
    -- Or
    --
    -- <@some_person_on_discord> @v?test
    --
    messager vars input
      | T.null input = return Nothing
      | T.head input /= '@' = messager vars (T.tail input)
      | input == "@" = return Nothing
      | T.head input == '@' = do
          case message' vars (T.tail input) of
            Right (Just ok) -> return (Just ok)
            Right Nothing -> messager vars (T.tail input)
            Left NoMonster -> return $ Just "No such monster."
      | otherwise = return Nothing

filterNewLines :: String -> String
filterNewLines = fmap (\ch -> if ch == '\n' || ch == '\r' then ' ' else ch)

-- substandard parsec "string" function for texts
stringT :: Stream s m Char => T.Text -> ParsecT s u m T.Text
stringT txt = T.pack <$> string (T.unpack txt)

data NoMonster = NoMonster
  deriving (Eq, Ord, Show, Read)

message' :: [V.Variant] -> T.Text -> Either NoMonster (Maybe T.Text)
message' variants input'
  | T.head input' == '@' = next (T.tail input') True
  | otherwise = next input' False
  where
    next input maximum_lev =
      case runParser (parser maximum_lev) () "line" input of
        Left errmsg -> Right $ Just $ T.pack $ filterNewLines $ show errmsg
        Right okay -> okay

    parser :: Bool -> T.Parser (Either NoMonster (Maybe T.Text))
    parser maximum_lev = do
      (variantStr, ignore) <-
        foldl
          ( \previoustry variant ->
              previoustry
                <|> try
                  ( (,)
                      <$> (stringT $ V.commandPrefix variant)
                      <*> try
                        (stringT "?")
                  )
          )
          (fail "")
          variants
          <|> ((,) <$> try (stringT "") <*> try (stringT "?"))
          <|> (return ("", ""))
      if ignore == "?"
        then doPart maximum_lev $ decideVariant variants variantStr
        else return (Right Nothing)

    doPart maximum_lev variant = do
      spaces
      rawMonsterName <- many anyChar
      let monsterName = (T.strip . T.pack) rawMonsterName
      when (T.length monsterName <= 0) $
        fail $
          ";I shall now launch the missiles that will cause "
            <> "serious international side effects."
      return $ case msms variant monsterName of
        Nothing -> Left NoMonster
        Just mon ->
          Right $
            Just $
              ( if T.toLower monsterName /= T.toLower mon
                  then
                    monsterName
                      <> " ~"
                      <> (T.pack $ show $ distT monsterName mon)
                      <> "~ "
                  else ""
              )
                <> lineMonsterInformation (fromJust $ V.monster variant mon)
      where
        msms =
          if maximum_lev then mostSimilarMonsterHalfSane else mostSimilarMonsterSane
