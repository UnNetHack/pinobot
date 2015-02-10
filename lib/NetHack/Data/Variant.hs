-- | This module describes the interface (as a data tyep) that some variant
-- should implement. See `Variant`.
--

{-# LANGUAGE OverloadedStrings #-}

module NetHack.Data.Variant
    ( Variant()
    , monster
    , allMonsterNames
    , commandPrefix
    , variant
    , loadVariant )
    where

import Control.Applicative
import Control.Monad.IO.Class
import qualified Data.ByteString as B
import Data.List ( find )
import qualified Data.Text as T
import Data.Yaml
import qualified NetHack.Data.Monster as MD

-- | Export a function that returns one of these to add a variant to the bot.
-- See `variant`.
data Variant = Variant { monster :: !(T.Text -> Maybe MD.Monster)
                       , allMonsterNames :: ![T.Text]
                       , commandPrefix :: T.Text }

instance FromJSON Variant where
    parseJSON (Object v) = do
        prefix <- v .: "prefix"
        monsters <- v .: "monsters"
        return Variant
            {
            commandPrefix = prefix
          , allMonsterNames = fmap MD.moName monsters
          , monster = \name -> find ((==) name . MD.moName) monsters
            }
    parseJSON _ = empty

-- Builds a `Variant` out of three properties.
variant :: (T.Text -> Maybe MD.Monster)   -- ^ Return a monster with the given
                                          -- name or `Nothing` if there is no 
                                          -- such monster.
        -> [T.Text]                       -- ^ The list of all monster names.
        -> T.Text                         -- ^ The command prefix for the IRC
                                          --   bot. E.g. "u" for UnNetHack.
        -> Variant
variant = Variant

-- Loads a variant from a YAML file.
loadVariant :: MonadIO m => FilePath -> m Variant
loadVariant fpath = liftIO $ do
    bs <- B.readFile fpath
    case decodeEither bs of
        Left err -> error err
        Right var -> return var

