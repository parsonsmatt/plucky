{-# language PatternSynonyms #-}

-- |
--
-- @since 0.0.1.0
module Data.Plucky.And where

import GHC.Generics (Generic)

-- |
--
-- @since 0.0.1.0
data And a b = And a b
    deriving stock (Eq, Show, Read, Generic, Ord)

-- |
--
-- @since 0.0.1.0
type (:*) = And

-- |
--
-- @since 0.0.1.0
pattern (:*) :: a -> b -> And a b
pattern (:*) a b = And a b
{-# COMPLETE (:*) #-}

infixr 7 :*

-- |
--
-- @since 0.0.1.0
class Has t env where
    get :: env -> t

-- |
--
-- @since 0.0.1.0
instance Has x x where
    get = id

-- |
--
-- @since 0.0.1.0
instance {-# overlapping #-} Has x (x :* y) where
    get (a :* _) = a

-- |
--
-- @since 0.0.1.0
instance {-# overlappable #-} Has x y => Has x (a :* y) where
    get (_ :* b) = get b
