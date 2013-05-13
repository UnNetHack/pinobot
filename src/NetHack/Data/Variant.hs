-- | This module describes the interface (as a data tyep) that some variant
-- should implement. See `Variant`.
--

module NetHack.Data.Variant
    ( Variant()
    , monster
    , allMonsterNames
    , commandPrefix
    , variant )
    where

import qualified Data.Text as T
import qualified NetHack.Data.Monster as MD

-- | Export a function that returns one of these to add a variant to the bot.
-- See `variant`.
data Variant = Variant { monster :: !(T.Text -> Maybe MD.Monster)
                       , allMonsterNames :: ![T.Text]
                       , commandPrefix :: String }

-- Builds a `Variant` out of three properties.
variant :: (T.Text -> Maybe MD.Monster)   -- ^ Return a monster with the given
                                          -- name or `Nothing` if there is no 
                                          -- such monster.
        -> [T.Text]                       -- ^ The list of all monster names.
        -> String                         -- ^ The command prefix for the IRC
                                          --   bot. E.g. "u" for UnNetHack.
        -> Variant
variant = Variant

