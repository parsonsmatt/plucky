{-# language GADTs, FlexibleContexts, DeriveAnyClass, ConstraintKinds #-}

-- | This module gives the
-- <https://www.parsonsmatt.org/2020/01/03/plucking_constraints.html Plucking Constraints>
-- approach towards extensible sum types.
--
-- The most common desire for extensible sum types is for exceptional
-- cases. The initial way to express an error is to define a sum type for
-- the possible failures:
--
-- @
-- data Error
--     = EmptyList
--     | KeyNotInMap
--     | FailToParse Text
--
-- program :: Text -> Either Error X
-- @
--
-- Unfortuanately, these sum types have problems. You can't handle "only"
-- the empty list case - the type @Error@ always represents that
-- possibility of failure.
--
-- @
-- handleEmptyList :: Error -> Either Error Int
-- handleEmptyList err =
--     case err of
--         EmptyList ->
--             Right 0
--         KeyNotInMap ->
--             Left KeyNotInMap
--         FailToParse ->
--             Left FailToParse
-- @
--
-- The return type still expresses the possiblity of @EmptyList@ related
-- failures, even though it's been handled. This is unsatisfying.
--
-- Instead, you could use nested 'Either' types to represent errors.
--
-- @
-- program :: Text -> Either (Either EmptyList (Either KeyNotInMap FailToParse) X
-- @
--
-- Now we can handle a single error.
--
-- @
-- handleEmptyList
--    :: Either EmptyList (Either KeyNotInMap FailToParse)
--    -> Either (Either KeyNotInMap FailToParse) Int
-- handleEmptyList err =
--     case err of
--         Left EmptyList ->
--             Right 0
--         Right (Left KeyNotInMap) ->
--             Left (Left KeyNotInMap)
--         Right (Right (FailToParse x)) ->
--             Left (Right (FailToParse x))
-- @
--
-- This is type safe - we can express exactly the errors we fail with.
-- We don't throw errors we don't need to throw, and we don't handle
-- errors that can't happen.
--
-- However, it's annoying! You have to wrap things in so many 'Left' and
-- 'Right constructors. And our function only works on an error type if
-- @EmptyList@ is the "leftmost" error - really, it should work on any
-- error type with @EmptyList@ anywhere at all, not just on the leftmost.
--
-- Plucking Constraints to the rescue!
--
-- Instead of producing concrete values, we'll produce polymorphic values.
-- This module defines @'Subtype' big lil@, a type class that expresses
-- that the @lil@ type is contained somewhere in the @big@ type. We write
-- @'inject' :: lil -> big@ to put the smaller type into the larger type.
--
-- Now, we'd express our program's error context like this:
--
-- @
-- program
--     :: ( 'Subtype' err EmptyList
--        , 'Subtype' err KeyNotInMap
--        , 'Subtype' err FailToParse
--        )
--     => Text
--     -> Either err X
-- @
--
-- For a familiar syntax, we also provide the ':<' operator:
--
-- @
-- program
--     :: ( EmptyList :< err
--        , KeyNotInMap :< err
--        , FailToParse :< err
--        )
--     => Text
--     -> Either err X
-- @
--
-- Instead of writing @'Left' EmptyList@, we'd write @'Left' ('inject'
-- EmptyList)@. Likewise, instead of @'Left' KeyNotInMap@, we'd write
-- @'Left' ('inject' KeyNotInMap)@. You can even write:
--
-- @
-- throw
--     :: ('MonadError' err m, e :< err)
--     => e -> m a
-- throw e = 'throwError' ('inject' e)
-- @
--
-- This nicely expresses how we can throw multiple possible error
-- constructors. How can we catch a single exception type?
--
-- That's where the datatype 'Or' comes in handy. 'Or' is similar to
-- 'Either', but it's defined in this library so we can provide custom
-- instances for the 'Exception' type class. The 'Subtype' class is defined
-- for the 'Or' type, such that any concrete type on the left gets
-- "plucked" out of the overall constraint. Let's look at an example.
--
-- @
-- program
--     :: forall err
--      . ( EmptyList :< err
--        , KeyNotInMap :< err
--        , FailToParse :< err
--        )
--     => Text
--     -> Either err X
-- @
--
-- The value @program@ is completely polymorphic in the error type,
-- provided the type satisfies the three constraints. This means we can
-- pick a more concrete, more specific type for @program@. The 'Or' type
-- (with an operator alias '||') is designed for plucking these 'Subtype'
-- constraints. Look at these alternate definitions for @program@:
--
-- @
-- program0
--     :: ( KeyNotInMap :< err
--        , FailToParse :< err
--        )
--     => Text
--     -> Either (EmptyList || err) X
-- program0 = program
--
-- program1
--     :: ( EmptyList :< err
--        , FailToParse :< err
--        )
--     => Text
--     -> Either (KeyNotInMap || err) X
-- program1 = program
--
-- program2
--     :: ( KeyNotInMap :< err
--        , EmptyList :< err
--        )
--     => Text
--     -> Either (FailToParse || err) X
-- program2 = program
--
-- program3
--     :: ( EmptyList :< err
--        )
--     => Text
--     -> Either (FailToParse || KeyNotInMap || err) X
-- program3 = program
--
-- program4
--     :: ( EmptyList :< err
--        )
--     => Text
--     -> Either (KeyNotInMap || FailToParse || err) X
-- program4 = program
--
-- program5
--     :: Text
--     -> Either (KeyNotInMap || FailToParse || EmptyList) X
-- program5 = program
--
-- program6
--     :: Text
--     -> Either (KeyNotInMap || FailToParse || EmptyList) X
-- program6 = program
--
-- program7
--     :: Text
--     -> Either (FailToParse || KeyNotInMap || EmptyList) X
-- program7 = program
-- @
--
-- All of these are totally fine, legal, and valid aliases for @program@.
-- Notice how the first few simply pluck a single type into the concrete
-- world - this is the common behavior we'll want to see.
--
-- Let's write @catch@ that works on a single error in the 'Either' type.
--
-- @
-- catch
--    :: Either (e || err) a
--    -> (e -> Either err a)
--    -> Either err a
-- catch action handler =
--     case action of
--         Left err ->
--             case err of
--                 This e ->
--                     handler e
--                 That err' ->
--                     Left err'
--         Right a ->
--             Right a
-- @
--
-- Now, given some input to @program@, we can handle the empty list case.
--
-- @
-- noEmptyList
--     :: (KeyNotInMap :< err, FailToParse :< err)
--     => Either err X
-- noEmptyList =
--     program "hello world"
--         `catch`
--             \EmptyList ->
--                 Right X
-- @
--
-- How does that work?! Well, @catch@ partially specializes the type of
-- @program@ based on the handler function. Let's write this out with
-- helpers in a @where@ block.
--
-- @
-- noEmptyList
--     :: forall err
--      . (KeyNotInMap :< err, FailToParse :< err)
--     => Either err X
-- noEmptyList =
--     action
--         `catch`
--             handler
--   where
--     handler :: EmptyList -> Either err X
--     handler EmptyList =
--         Right X
--
--     action :: Either (EmptyList || err) X
--     action =
--         program "hello world"
-- @
--
-- But we're not limited to merely handling errors. We can translate errors
-- into other error types. This works flawlessly.
--
-- @
-- swapError
--     :: (SomeOtherError :< err)
--     => Either (SomeError || err) a
--     -> Either err a
-- swapError someProgram =
--     someProgram
--         `catch` \SomeError ->
--             Left (inject SomeOtherError)
-- @
--
-- This function swaps errors in the input with a new type of error.
--
-- @since 0.0.1.0
module Data.Plucky.Or where

import Control.Applicative (Alternative(..))
import Control.Exception hiding (TypeError)
import GHC.Generics (Generic)
import GHC.TypeLits

-- | A datatype used to pluck constraints from the 'Subtype' class.
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
class (Subtype big lil) => lil :< big
instance (Subtype big lil) => lil :< big

-- | An alias for ':<' that also marks the large type as an instance of
-- 'Exception'. Useful for throwing and catching exceptions.
type a :<! b = (a :< b, Exception b)

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
