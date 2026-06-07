{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- A small module that provides the little lens-operations Pinobot needs.
--
-- Avoids bringing in the large 'lens' dependency.
--

module IRC.MicroLens
  ( (.~),
    (%~),
    (^.),
    (&),
  )
where

import Data.Coerce
import Data.Function ((&))
import Data.Functor.Identity

infixl 8 ^.

infixr 4 .~, %~

infixr 9 #.

newtype Const a (b :: k) = Const {getConst :: a}
  deriving (Functor)

type ASetter s t a b = (a -> Identity b) -> s -> Identity t

type Getting r s a = (a -> Const r a) -> s -> Const r s

{-# INLINE (.~) #-}
(.~) :: ASetter s t a b -> b -> s -> t
(.~) = set

{-# INLINE set #-}
set :: ASetter s t a b -> b -> s -> t
set l b = runIdentity #. l (\_ -> Identity b)

{-# INLINE (%~) #-}
(%~) :: ASetter s t a b -> (a -> b) -> s -> t
(%~) = over

over :: ASetter s t a b -> (a -> b) -> s -> t
over = coerce

{-# INLINE (^.) #-}
(^.) :: s -> Getting a s a -> a
s ^. l = getConst (l Const s)

{-# INLINE (#.) #-}
(#.) :: (Coercible c b) => (b -> c) -> (a -> b) -> (a -> c)
(#.) _ = coerce (\x -> x :: b) :: forall a b. (Coercible b a) => a -> b
