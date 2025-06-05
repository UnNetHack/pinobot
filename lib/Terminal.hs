{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiWayIf #-}

-- | Dealing with the terminal (as in the terminal window someone might run
-- Pinobot in, and see stdout and stderr of it).
--
-- To be a serious program in 2025, you have to have pompousness in your
-- terminal output. Colors. Unicode. Maybe emojis? I'm not ready yet for
-- emojis but we'll put that to ruminate on the back of my mind.
--
-- As of writing of this, my concern was how to detect if a terminal
-- understands Unicode (or I guess more specifically UTF-8). And that is how
-- guessIfterminalDoesUTF8 came to be.
--
-- Also Pinobot recognizes one environment variable of its own:
-- NO_PINOBOT_POMPOUSNESS, if set, makes Pinobot assume no Unicode no fancy
-- terminal stuff.
--

module Terminal
  (
  -- * Inspecting the terminal and environment
    guessIfterminalDoesUTF8
  , canWeUsePompousTerminalStuff
  , getTerminalSize
  , TerminalSizeResult(..)
  -- * Pinobot logo
  , renderPinobotLogo
  -- * Pompous table rendering
  , renderPompousTable
  , TableCell(..)
  , text
  , TableDrawingCharacterSet(..)
  , getTableDrawingCharacterSet )
  where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.State.Strict
import Data.Colour
import Data.Colour.SRGB
import Data.Foldable
import Data.Map.Strict ( Map )
import qualified Data.Map.Strict as M
import Data.Maybe ( catMaybes, isJust, fromMaybe )
import Data.Set ( Set )
import qualified Data.Set as S
import Data.String
import Data.Traversable ( for )
import Data.Text ( Text )
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL hiding ( singleton )
import qualified Data.Text.Lazy.Builder as TL
import PinobotLogo ( pinobotLogoBytes )
import System.Console.ANSI hiding ( getTerminalSize ) -- the important package doing the heavy lifting (from `ansi-terminal` package)
import qualified System.Console.ANSI as ANSI
import System.Environment
import System.IO ( stdout)

isPinobotPompousnessExplicitlyTurnedOff :: MonadIO m => m Bool
isPinobotPompousnessExplicitlyTurnedOff =
  liftIO $ isJust <$> lookupEnv "NO_PINOBOT_POMPOUSNESS"

-- | Tries to guess if the terminal the calling process is running in supports
-- unicode.
--
-- This investigates LANG, LC_ALL and LC_CTYPE environment variables. If any of
-- them contain "utf-8" somewhere, we guess yes. Otherwise no.
guessIfterminalDoesUTF8 :: MonadIO m => m Bool
guessIfterminalDoesUTF8 = liftIO $ do
  turned_off_explicitly <- isPinobotPompousnessExplicitlyTurnedOff
  if turned_off_explicitly
    then pure False

    else do candidate_envs <- fmap catMaybes $ for ["LANG", "LC_ALL", "LC_CTYPE"] $ \env_key ->
              fmap T.pack <$> lookupEnv env_key
            pure $ any hasUTF8Inside candidate_envs
 where
  hasUTF8Inside :: Text -> Bool
  hasUTF8Inside candidate_value = "utf-8" `T.isInfixOf` T.toLower candidate_value

-- | Tries to determine if we should use unicode, fancy colors, other pomp, or
-- should we just assume dumb ASCII output.
--
-- This does unicode check and then hSupportsANSI from ansi-terminal package
-- which does some other checks (e.g. we do we have a tty); I read the
-- documentation and it is quite sensible.
canWeUsePompousTerminalStuff :: MonadIO m => m Bool
canWeUsePompousTerminalStuff = liftIO $ do
  turned_off_explicitly <- isPinobotPompousnessExplicitlyTurnedOff
  if turned_off_explicitly
    then pure False
    else do does_utf8 <- guessIfterminalDoesUTF8
            if does_utf8
              then hSupportsANSI stdout
              else pure False

data TerminalSizeResult
  = NoTerminal
  | TerminalSize !Int !Int   -- rows columns
  deriving ( Eq, Ord, Show, Read )

-- | Gets the terminal size (if there is a terminal).
--
-- This just wraps ansi-terminal packages hGetTerminalSize but a bit more
-- Pinobotish interface.
getTerminalSize :: MonadIO m => m TerminalSizeResult
getTerminalSize = liftIO $ do
  turned_off_explicitly <- isPinobotPompousnessExplicitlyTurnedOff
  if turned_off_explicitly
    then pure NoTerminal
    else toTerminalSizeResult <$> ANSI.getTerminalSize
 where
  toTerminalSizeResult Nothing = NoTerminal
  toTerminalSizeResult (Just (rows, cols)) = TerminalSize rows cols

-- | For pompous tables (see `renderPompousTable`), this records what
-- characters we can use to make the tables.
--
-- If fancy terminal is supported, we can use unicode and colors. Otherwise we
-- use ugly ASCII.
data TableDrawingCharacterSet = TableDrawingCharacterSet
  { topLeftCornerCharacter :: !Char
  , topRightCornerCharacter :: !Char
  , bottomLeftCornerCharacter :: !Char
  , bottomRightCornerCharacter :: !Char
  , lineIntersectionCharacter :: !Char
  , horizontalDash :: !Char
  , verticalDash :: !Char
  , tpieceTop :: !Char    -- On top row, the character to use when joined from below too (T-shape).
  , tpieceLeft :: !Char   -- Same for these others, just for different sides.
  , tpieceRight :: !Char
  , tpieceBottom :: !Char
  , useRGB :: !Bool }
  deriving ( Eq, Ord, Show, Read )

boringASCIITableDrawingCharacterSet :: TableDrawingCharacterSet
boringASCIITableDrawingCharacterSet = TableDrawingCharacterSet
  { topLeftCornerCharacter = '+'
  , topRightCornerCharacter = '+'
  , bottomLeftCornerCharacter = '+'
  , bottomRightCornerCharacter = '+'
  , lineIntersectionCharacter = '+'
  , horizontalDash = '-'
  , verticalDash = '|'
  , tpieceTop = '+'
  , tpieceLeft = '+'
  , tpieceRight = '+'
  , tpieceBottom = '+'
  , useRGB = False }

pompousUnicodeTableDrawingCharacterSet :: TableDrawingCharacterSet
pompousUnicodeTableDrawingCharacterSet = TableDrawingCharacterSet
  { topLeftCornerCharacter = '╔'
  , topRightCornerCharacter = '╗'
  , bottomLeftCornerCharacter = '╚'
  , bottomRightCornerCharacter = '╝'
  , lineIntersectionCharacter = '╬'
  , horizontalDash = '═'
  , verticalDash = '║'
  , tpieceTop = '╦'
  , tpieceLeft = '╠'
  , tpieceBottom = '╩'
  , tpieceRight = '╣'
  , useRGB = True }

-- | Gets an appropriate table drawing character set that likely will render
-- corrrectly in the current terminal.
getTableDrawingCharacterSet :: MonadIO m => m TableDrawingCharacterSet
getTableDrawingCharacterSet = liftIO $ do
  can_use <- canWeUsePompousTerminalStuff
  pure $ if can_use
    then pompousUnicodeTableDrawingCharacterSet
    else boringASCIITableDrawingCharacterSet

-- Sometimes refers to Nth row or Nth column by character. Sometimes refers to
-- Nth *table* row or Nth *table* column. Who needs consistency. Also not used
-- at all sometimes (you see Int instead).
type Row = Int
type Col = Int

-- | Used in renderPompousTable. HorizontalLine on any column in a row triggers
-- creation of a horizontal line and not rendering the row otherwise.
data TableCell = OutOfRange | Text !Text | HorizontalLine
  deriving ( Eq, Ord, Show, Read )

-- `text` for sure will not clash with any existing names...
text :: String -> TableCell
text str = Text $ T.pack str

instance IsString TableCell where
  fromString str = Text (T.pack str)

cellText :: TableCell -> Text
cellText (Text txt) = txt
cellText _ = ""

cellLength :: TableCell -> Int
cellLength cell = T.length $ cellText cell

data RowPlacement = TopRow | MiddleRow | BottomRow
  deriving ( Eq, Ord, Show, Read )

newtype RenderMonad a = RenderMonad (State RenderState a)
  deriving ( Monad, Applicative, Functor )

-- State that tracks what string we have built, and our current location in it.
data RenderState = RenderState
  { renderCol :: !Col   -- ^ Keep track of current column
  , renderRow :: !Row   -- ^ Keep track of current row
  , getRGBAtFunc :: !(Maybe (Row -> Col -> Colour Float))    -- ^ What color to use for certain location.
  , builder :: !TL.Builder -- ^ Text we are building.
  , usedRGB :: !Bool    -- ^ Set to true if any terminal colors/fancy things were used.
  , rgbTurnedOn :: !Bool    -- ^ Use to turn RGB on and off temporarily. Starts out True. No RGB will be used unless getRGBAtFunc is in use.
  , characterSet :: !TableDrawingCharacterSet
  }

initialRenderState :: RenderState
initialRenderState = RenderState
  { renderCol = 0
  , renderRow = 0
  , builder = mempty
  , getRGBAtFunc = Nothing
  , rgbTurnedOn = True
  , usedRGB = False
  , characterSet = boringASCIITableDrawingCharacterSet }

-- This does not check for wide characters. Every unicode code point = one
-- column (according to this code). Could try looking into wcwidth() or other
-- ways (afaik this is a complicated problem to do if you really want to handle
-- all corner cases and stuff).
tellChar :: Char -> RenderMonad ()
tellChar '\n' = RenderMonad $
  modify $ \st -> st { renderCol = 0, renderRow = renderRow st + 1, builder = builder st <> (TL.singleton '\n') }
tellChar ch = RenderMonad $ do
  snapshot <- get
  let col = renderCol snapshot
      row = renderRow snapshot

  payload <- case (getRGBAtFunc snapshot, useRGB (characterSet snapshot), rgbTurnedOn snapshot) of
    (_, _, False) -> pure $ TL.singleton ch
    (_, False, _) -> pure $ TL.singleton ch
    (Nothing, _, _) -> pure $ TL.singleton ch
    (Just get_func, _, _) -> do
      modify $ \st -> st { usedRGB = True }
      let rgb = get_func row col
      pure $ TL.fromString (setSGRCode [SetRGBColor Foreground rgb]) <> TL.singleton ch

  payload `seq` (modify $ \st -> st { renderCol = renderCol st + 1, builder = builder st <> payload })

turnOnRGB :: RenderMonad ()
turnOnRGB = RenderMonad $ do
  modify $ \st -> st { rgbTurnedOn = True }

turnOffRGB :: RenderMonad ()
turnOffRGB = RenderMonad $ do
  modify $ \st ->
    st { rgbTurnedOn = False
       , builder = if usedRGB st
           then builder st <> (TL.fromString $ setSGRCode [Reset])
           else builder st }

withRGBTurnedOff :: RenderMonad a -> RenderMonad a
withRGBTurnedOff action = do
  old_value <- RenderMonad $ rgbTurnedOn <$> get
  turnOffRGB
  result <- action
  if old_value
    then turnOnRGB
    else turnOffRGB
  pure result

tell :: Text -> RenderMonad ()
tell txt = T.foldlM' (\_ ch -> tellChar ch) () txt

execRenderMonad :: RenderMonad () -> Text
execRenderMonad (RenderMonad stateful) =
  let final_state = execState action initialRenderState
   in TL.toStrict $ TL.toLazyText $ builder final_state
 where
  action :: State RenderState ()
  action = do
    stateful
    -- Reset terminal colors if they were in use.
    snapshot <- get
    when (usedRGB snapshot) $
      modify $ \st -> st { builder = builder st <> (TL.fromString $ setSGRCode [Reset]) }

setRenderMonadCharacterSet :: TableDrawingCharacterSet -> RenderMonad ()
setRenderMonadCharacterSet character_set = RenderMonad $ modify $ \st -> st { characterSet = character_set }

setGetRGBFunc :: (Row -> Col -> Colour Float) -> RenderMonad ()
setGetRGBFunc func = RenderMonad $ modify $ \st -> st { getRGBAtFunc = Just func }

-- | Render the Pinobot logo, with a style consistent with
-- TableDrawingCharacterSet.
--
-- May return empty string, if the logo probably could not be drawn. It
-- inspects the environment (along with checking wiht
-- TableDrawingCharacterSet).
--
-- Any self-respecting program in 2025 has a giant ass logo for no reason.
-- This is Pinobot's.
--
-- The string will have a newline (if it is not empty).
renderPinobotLogo :: MonadIO m => TableDrawingCharacterSet -> m Text
renderPinobotLogo ch_set = liftIO $ do
  can_we_use <- canWeUsePompousTerminalStuff
  sz <- getTerminalSize

  if | not (useRGB ch_set) -> pure ""
     | not can_we_use -> pure ""
     | otherwise -> case sz of
         TerminalSize term_h term_w | term_h >= 20 && term_w >= 85 ->
           pure $ (T.decodeUtf8 pinobotLogoBytes) <> T.pack (setSGRCode [Reset])
         _ -> pure ""

-- | Render a ridiculously pompous table and return it as a string (that could
-- then be printed to stdout).
--
-- Automatically sizes columns.
renderPompousTable :: TableDrawingCharacterSet  -- ^ See `getTableDrawingCharacterSet`
                   -> (Row -> Col -> TableCell) -- ^ Get text to be in a column.
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
                   -> Text
renderPompousTable ch_set get_cell_content = execRenderMonad $ unless (M.null cell_contents) $ do
  setRenderMonadCharacterSet ch_set
  setGetRGBFunc getRGBAt

  tellHorizontalLine TopRow
  -- Header
  tellCells 0
  tellHorizontalLine MiddleRow
  -- Cells
  for_ [1..n_cells_height-1] $ \row_idx ->
    tellCells row_idx
  tellHorizontalLine BottomRow
 where
  -- Shortcuts to character set.
  topleft_ch = topLeftCornerCharacter ch_set
  topright_ch = topRightCornerCharacter ch_set
  bottomleft_ch = bottomLeftCornerCharacter ch_set
  bottomright_ch = bottomRightCornerCharacter ch_set
  intersection_ch = lineIntersectionCharacter ch_set
  horizontal_dash_ch = horizontalDash ch_set
  vertical_dash_ch = verticalDash ch_set
  left_tpiece_ch = tpieceLeft ch_set
  right_tpiece_ch = tpieceRight ch_set
  top_tpiece_ch = tpieceTop ch_set
  bottom_tpiece_ch = tpieceBottom ch_set

  dist2d :: Float -> Float -> Float -> Float -> Float
  dist2d x1 y1 x2 y2 = sqrt $ (x1 - x2)**2.0 + (y1 - y2)**2.0

  -- Gets the RGB color this part of the table should have. For pompousness
  -- purposes. Of course.
  getRGBAt :: Row -> Col -> Colour Float
  getRGBAt row col =
    -- Nice green on top-left corner, kinda orangeish in the middle, purpleish
    -- on bottom-right. For maximum rainbow fruit salad power.
    --
    -- Interpolation is done in whatever colorspace the 'colour' package uses
    -- which wasn't entirely clear to me when I used about 8 seconds to skim it
    -- documentation. It is vibrant enough though for the pompous purposes.
    --
    -- affineCombo does the interpolation.
    let top_left_color = sRGB24 0 255 0 :: Colour Float
        middle_color = sRGB24 255 128 0 :: Colour Float
        bottom_right_color = sRGB24 180 60 255 :: Colour Float

        topleft_x = 0 :: Float
        topleft_y = 0 :: Float

        bottomright_x = fromIntegral $ total_width - 1 :: Float
        bottomright_y = fromIntegral $ total_height - 1 :: Float

        middle_x = (topleft_x + bottomright_x) * 0.5
        middle_y = (topleft_x + bottomright_x) * 0.5

        dcol = fromIntegral col :: Float
        drow = fromIntegral row :: Float

        weight_topleft' = dist2d dcol drow topleft_x topleft_y
        weight_middle' = dist2d dcol drow middle_x middle_y
        weight_bottom' = dist2d dcol drow bottomright_x bottomright_y

        total_weight' :: Float
        total_weight' = weight_topleft' + weight_middle' + weight_bottom'
        total_weight :: Float
        total_weight = if total_weight' > 0.0
                         then total_weight'
                         else 1.0

        weight_topleft = weight_topleft' / total_weight
        weight_middle = weight_middle' / total_weight
        weight_bottom = weight_bottom' / total_weight

        dummy = sRGB24 0 0 0 :: Colour Float

        interpolated_color = affineCombo [(weight_topleft, top_left_color)
                                         ,(weight_middle, middle_color)
                                         ,(weight_bottom, bottom_right_color)]
                                         dummy :: Colour Float

     in interpolated_color

  tellHorizontalLine :: RowPlacement -> RenderMonad ()
  tellHorizontalLine row_placement = do
    if | row_placement == TopRow -> tellChar topleft_ch
       | row_placement == BottomRow -> tellChar bottomleft_ch
       | otherwise -> tellChar left_tpiece_ch

    for_ [0..total_width - 2 - 1] $ \nth_dash -> do
      let col = nth_dash + 1
      -- Special cases for top and bottom row
      if | row_placement == TopRow ->
             if col `S.member` starry_columns
               then tellChar top_tpiece_ch
               else tellChar horizontal_dash_ch
         | row_placement == BottomRow -> 
             if col `S.member` starry_columns
               then tellChar bottom_tpiece_ch
               else tellChar horizontal_dash_ch
         | otherwise ->
             if col `S.member` starry_columns
               then tellChar intersection_ch
               else tellChar horizontal_dash_ch

    if | row_placement == TopRow -> tellChar topright_ch
       | row_placement == BottomRow -> tellChar bottomright_ch
       | otherwise -> tellChar right_tpiece_ch

    tell "\n"

  -- Renders one row, putting pipes in the right places and respecting column
  -- widths.
  tellCells :: Int -> RenderMonad ()
  tellCells row = do
    if row `S.member` horizontal_lines
      then tellHorizontalLine MiddleRow
      else tellCellsLikeActualTextCellAndNotAPompousLine row

  tellCellsLikeActualTextCellAndNotAPompousLine :: Int -> RenderMonad ()
  tellCellsLikeActualTextCellAndNotAPompousLine row = do
    for_ [0..n_cells_width-1] $ \column_idx -> do
      tellChar vertical_dash_ch
      let text_content = fromMaybe "" $ fmap cellText $ M.lookup (column_idx, row) cell_contents
          column_size = fromMaybe 0 $ M.lookup column_idx column_sizes
          text_len = T.length text_content
      tellChar ' '
      withRGBTurnedOff $ tell text_content
      tell $ T.replicate (column_size - text_len - 1) " "

    tellChar vertical_dash_ch
    tell "\n"

  n_cells_width :: Int
  n_cells_width = if M.null cell_contents then 0 else
    (maximum $ fmap fst $ M.keys cell_contents) + 1

  n_cells_height :: Int
  n_cells_height = if M.null cell_contents then 0 else
    (maximum $ fmap snd $ M.keys cell_contents) + 1

  -- set of rows that should be horizontal lines
  horizontal_lines :: Set Row
  horizontal_lines =
    S.fromList $
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

  -- ditto for height
  total_height = if M.null column_sizes then 0 else
    -- top, bottom and header horizontal lines
    3 +
    -- any manually set horizontal lines
    S.size horizontal_lines +
    -- text rows
    n_cells_height

  cell_contents :: Map (Col, Row) TableCell
  cell_contents = go 0 0 mempty
   where
    go :: Col -> Row -> Map (Col, Row) TableCell -> Map (Col, Row) TableCell
    go !x !y !accum = case get_cell_content y x of
      OutOfRange | x > 0 -> go 0 (y+1) accum
      OutOfRange | x == 0 -> accum
      OutOfRange -> accum -- should be unreachable (x cannot be negative)
      Text txt -> go (x+1) y (M.insert (x, y) (Text txt) accum)
      HorizontalLine -> go (x+1) y (M.insert (x, y) HorizontalLine accum)
