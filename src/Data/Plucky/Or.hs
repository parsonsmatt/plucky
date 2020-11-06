-- |
--
-- @since 0.0.1.0
module Data.Plucky.Or where

import Control.Applicative (Alternative(..))
import Control.Exception hiding (TypeError)
import GHC.Generics (Generic)
import GHC.TypeLits

-- |
--
-- @since 0.0.1.0
data Or a b
    = This a
    | That b
    deriving stock (Eq, Show, Read, Generic, Ord)

-- |
--
-- @since 0.0.1.0
type (||) = Or
infixr 6 ||

-- |
--
-- @since 0.0.1.0
instance (Exception a, Exception b) => Exception (Or a b) where
    toException = \case
        This a -> toException a
        That a -> toException a

    fromException e =
        This <$> fromException e <|> That <$> fromException e

    displayException = \case
        This a -> displayException a
        That a -> displayException a

-- |
--
-- @since 0.0.1.0
class (Subtype big lil, Exception big) => lil :< big
instance (Subtype big lil, Exception big) => lil :< big

-- |
--
-- @since 0.0.1.0
class Subtype large single where
    inject :: single -> large

-- |
--
-- @since 0.0.1.0
instance {-# Overlappable #-} Subtype a a where
    inject = id

-- |
--
-- @since 0.0.1.0
instance {-# Overlapping #-} Subtype (a || b) a where
    inject = This

-- |
--
-- @since 0.0.1.0
instance {-# Overlappable #-} (Subtype b c) => Subtype (a || b) c where
    inject = That . inject

-- |
--
-- @since 0.0.1.0
instance {-# overlappable #-} (TypeError (SubtypeErrorMsg a b)) => Subtype a b where
    inject = undefined

-- |
--
-- @since 0.0.1.0
type SubtypeErrorMsg a b =
    'Text "The type " ':<>: 'ShowType b ':<>: 'Text " is not a subtype of " ':<>: 'ShowType a
