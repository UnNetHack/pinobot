{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE AutoDeriveTypeable #-}

module PinobotSafe
    ( Num(..), Int, Integer, Float, Double, undefined
    , (&&), (||), Bool
    , not
    , otherwise
    , Maybe
    , maybe
    , Either
    , either
    , Ordering
    , Char
    , T.Text()
    , fst, snd
    , Eq(..)
    , Ord(..)
    , Show()
    , show
    , showP
    , showType
    , Read(..)
    , Enum(..)
    , Bounded(..)
    , Rational
    , Ratio(..)
    , Real(..)
    , Integral(..)
    , Fractional(..)
    , Floating(..)
    , RealFrac(..)
    , RealFloat(..)
    , String(..)
    , (%)
    , numerator
    , denominator
    , approxRational
    , subtract
    , flip
    , even, odd, gcd, lcm, (^), (^^), fromIntegral, realToFrac
    , Monad()
    , (>>=)
    , (>>)
    , (=<<)
    , join
    , return
    , Applicative(..)
    , Functor(..)
    , Category(..)
    , Foldable(..)
    , Traversable(..)
    , Alternative(..)
    , Monoid(..)
    , Semigroup(..)
    , const
    , ($)
    , until
    , error
    , seq
    , ($!)
    , (++)
    , map
    , filter
    , foldrM
    , foldlM
    , traverse_
    , for_
    , sequenceA_
    , asum
    , toList
    , concat
    , concatMap
    , and
    , or
    , any
    , sum
    , product
    , maximum
    , maximumBy
    , minimum
    , minimumBy
    , elem
    , notElem
    , find
    , for
    , Typeable()
    , typeOf
    , T.pack
    , T.unpack
    , T.strip
    , module Ex )
    where

import Prelude hiding ( IO, (.), id, concat, and, or, concatMap, minimum
                      , maximum, product, elem, any, sum, notElem, show )
import qualified Prelude as P
import Data.Ratio
import Control.Monad
import Control.Category
import Control.Applicative
import Data.Typeable
import Data.Foldable
import Data.Traversable
import Data.Semigroup
import NetHack.Data.Dice as Ex
import NetHack.Data.Variant as Ex
import NetHack.Data.Monster as Ex

import qualified Data.Text as T

showType :: Typeable a => a -> T.Text
showType = show . typeOf

show :: Show a => a -> T.Text
show = T.pack . P.show

showP :: Show a => a -> String
showP = P.show

