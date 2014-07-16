{-# LANGUAGE DeriveDataTypeable, MultiWayIf #-}

module NetHack.Data.Dice
    ( Dice( .. )
    , d
    , avg
    , ld
    , rd )
    where

import Data.Typeable

data Dice = Dice !Integer !Integer
            deriving ( Eq, Show, Ord, Typeable )

d :: Integer -> Integer -> Dice
d = Dice

ld :: Dice -> Integer
ld (Dice l _) = l

rd :: Dice -> Integer
rd (Dice _ r) = r

avg :: Fractional a => Dice -> a
avg (Dice l r) = (fromIntegral r+1)/2*fromIntegral l

