-- | This module describes the interface (as a data tyep) that some variant
-- should implement. See `Variant`.
--

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module NetHack.Data.Variant
    ( Variant()
    , monster
    , allMonsterNames
    , numMonsters
    , commandPrefix
    , variant
    , variantName
    , setVariantName
    , source
    , lastUpdated
    , loadVariant )
    where

import qualified Codec.Compression.GZip as GZip
import Control.Applicative
import Control.DeepSeq
import Control.Monad.IO.Class
import Control.Exception
import qualified Data.ByteString.Lazy as BL
import Data.List ( find )
import qualified Data.Text as T
import Data.Yaml
import qualified NetHack.Data.Monster as MD
import System.IO.Error

-- | A variant (one of those files in variants/*.yaml) but in the form of a
-- loaded value. see `variant` function.
data Variant = Variant { monster :: !(T.Text -> Maybe MD.Monster)
                       , allMonsterNames :: ![T.Text]
                       , commandPrefix :: T.Text
                       , source :: Maybe T.Text -- ^ "source:" field in .yaml
                       , variantName :: !T.Text -- ^ "variant:" field in .yaml.
                       , lastUpdated :: Maybe T.Text -- ^ "last_updated:" field in yaml.
                       }

setVariantName :: T.Text -> Variant -> Variant
setVariantName name v = v { variantName = name }

numMonsters :: Variant -> Int
numMonsters = length . allMonsterNames

instance FromJSON Variant where
    parseJSON (Object v) = do
        prefix <- v .: "prefix"
        monsters <- v .: "monsters"
        parsed_source <- v .:? "source"
        variant_name <- v .: "variant"
        last_updated <- v .:? "last_updated"

        return Variant
            { commandPrefix = prefix
            , allMonsterNames = fmap MD.moName monsters
            , monster = \name -> find ((==) name . MD.moName) monsters
            , source = parsed_source
            , variantName = variant_name
            , lastUpdated = last_updated }
    parseJSON _ = empty

-- Builds a `Variant` out of three properties.
variant :: (T.Text -> Maybe MD.Monster)   -- ^ Return a monster with the given
                                          -- name or `Nothing` if there is no 
                                          -- such monster.
        -> [T.Text]                       -- ^ The list of all monster names.
        -> T.Text                         -- ^ The command prefix for the IRC
                                          --   bot. E.g. "u" for UnNetHack.
        -> Maybe T.Text                   -- ^ The source where info came from
                                          -- (e.g. git commit if it's a variant
                                          -- that's on git).
                                          -- ^ Name of the variant. (Basename
                                          -- of the .yaml file usually)
        -> T.Text                         -- ^ Variant name
        -> Maybe T.Text                   -- ^ Last updated
        -> Variant
variant = Variant

-- Loads a variant from a YAML file.
loadVariant :: MonadIO m => FilePath -> m Variant
loadVariant fpath = liftIO $ do
  maybe_bs <- try $ BL.readFile fpath
  -- FIXME: this nested case is a mess but I am one lazy programmer
  case maybe_bs of
    Left (_err :: IOError) | isDoesNotExistError _err -> do
      -- Try load the compressed version if plain version doesn't exist.
      let fpath_gz = fpath <> ".gz"
      maybe_compressed_bs <- try $ BL.readFile fpath_gz
      case maybe_compressed_bs of
        Left (_err :: IOError) | isDoesNotExistError _err ->
          error $ "Cannot find either " <> fpath <> " or " <> fpath_gz
        Left err -> throwIO err
        Right compressed_bs -> do
          let bs_lazy = GZip.decompress compressed_bs
          -- GZip.decompress is lazy, force the bytestring so that we error out here
          -- if the file itself is bad/corrupted/whatever.
          bs <- evaluate $ force bs_lazy
          goDecode bs
    Left err -> throwIO err
    Right bs -> goDecode bs
 where
  goDecode :: BL.ByteString -> IO Variant
  goDecode bs = case decodeEither' (BL.toStrict bs) of
    Left err -> error $ "Parsing variant .yaml failed: " <> show err
    Right var -> return var

