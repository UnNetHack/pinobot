module NetHack.Data.Dice
    ( Dice( .. ) )
    where

data Dice = Dice !Int !Int
            deriving ( Eq, Show, Ord )

