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
    , permutations
    , head
    , tail
    , last
    , init
    , null
    , length
    , reverse
    , intersperse
    , transpose
    , subsequences
    , scanl
    , scanl1
    , scanr
    , scanr1
    , iterate
    , unfoldr
    , repeat
    , replicate
    , cycle
    , take
    , drop
    , splitAt
    , takeWhile
    , dropWhile
    , span
    , break
    , group
    , inits
    , tails
    , lookup
    , (!!)
    , zip
    , zip3
    , zip4
    , zip5
    , zip6
    , zip7
    , zipWith
    , zipWith3
    , zipWith4
    , zipWith5
    , zipWith6
    , zipWith7
    , unzip
    , unzip3
    , unzip4
    , unzip5
    , unzip6
    , unzip7
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
import Data.Function ( fix )
import Data.List hiding ( elem, find, notElem, minimum, minimumBy, maximum, maximumBy, product, concatMap, concat, and, or, any, sum )
import NetHack.Data.Dice as Ex
import NetHack.Data.Variant as Ex
import NetHack.Data.Monster as Ex
import Control.Monad.Fix as Ex
import Control.Comonad as Ex
import Control.Monad.Trans.Reader as Ex hiding ( liftCatch, liftCallCC, liftListen )
import Control.Monad.Trans.State.Lazy as Ex hiding ( liftPass, liftListen, liftCatch, liftCallCC )
import Control.Monad.Trans.Writer.Lazy as Ex hiding ( liftCatch, liftCallCC )
import Control.Monad.Trans.Maybe as Ex hiding ( liftListen, liftCallCC, liftPass )
import Control.Monad.Trans.Except as Ex hiding ( liftListen, liftCallCC, liftPass )
import Control.Comonad.Trans.Store as Ex
import Control.Monad.Free as Ex
import Data.Functor.Identity as Ex
import qualified Data.Text as T

showType :: Typeable a => a -> T.Text
showType = show . typeOf

show :: Show a => a -> T.Text
show = T.pack . P.show

showP :: Show a => a -> String
showP = P.show

