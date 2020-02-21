{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

-- | In <https://www.parsonsmatt.org/2020/01/03/plucking_constraints.html Plucking Constraints>,
-- I described a technique where you can collect constraints and then pluck
-- them off one-by-one. In order to pluck them off, you need a type class
-- and a datatype that supports concretely matching one of the constraints
-- and delegating the rest of them.
--
-- I hint that errors can be handled in this way, and this library
-- demonstrates the technique. I do not make any guarantee of
-- prodution-readiness or maintainability, and indeed this technique does
-- not work with @mtl@'s 'MonadError', due to the functional dependency. So
-- this is primarily useful in a concrete 'ExceptT' monad or with 'Either'
-- directly.
module Data.Either.Plucky
    ( -- * Throwing Errors
      throw
    , rethrow
    , throwT
      -- * Catching Errors
    , catch
    , catchOne
    , catchT
    , catchOneT
      -- * Concise Type Signature Helper
    , OneOf
    -- * The magical internal guts!

    -- | This type class has several instances that are well-documented and
    -- describe how the library works and why. If you're curious about the
    -- way this library works, read this up.
    , ProjectError(..)
    ) where

import           Control.Monad.Error.Class
import           Control.Monad.Trans.Except
import           GHC.Exts
import           GHC.TypeLits

-- | This type class is used to create the delegated values. It's an
-- internal implementation detail, and you should not write any instances
-- of it.
--
-- The real meat is in the instance documentation, so make sure you check
-- that out.
--
-- TODO: rename this class
--
-- @since 0.0.0.0
class ProjectError large single where
    -- | The type class has a single function which is used to shove
    -- a @single@ value into a @large@ value that contains it somehow. We
    -- use this to tell GHC how to organize our 'Either's so that
    -- everything magically works out.
    --
    -- @since 0.0.0.0
    putError :: single -> large

-- | This instance is the most base of base cases. A value may just be
-- itself! This case can trigger whenever you only have a single constraint
-- on a type. Let's consider the following code:
--
-- @
-- data X = X
--
-- value :: ('ProjectError' a X) => 'Either' a ()
-- value = 'Left' ('putError' X)
-- @
--
-- There's only a single constraint on the error type. If we want to, we
-- can write a specialized version of @value@ that picks the @large@ type
-- to be equal to the @single@ thing it contains.
--
-- @
-- value' :: 'Either' X ()
-- value' = value
-- @
--
-- Now, there's no weird polymorphism or type variables, and we can just
-- pattern match directly on the value. However, we can skip the manual
-- specializing of the type, and pattern match directly on the value:
--
-- @
-- main :: IO ()
-- main = do
--     case value of
--         Left X -> print "it was an X"
--         Right () -> pure ()
-- @
--
-- @since 0.0.0.0
instance {-# OVERLAPPABLE #-} ProjectError a a where
    putError = id

-- | This is another terminating case in 'ProjectError'. If we are asking
-- for the 'Left' in our type, then we can stop recursion and return the
-- 'Left'. Let's consider a value with three constraints on it:
--
-- @
-- data A = A
-- data B = B
-- data C = C
--
-- three
--     :: ('ProjectError' e A, 'ProjectError' e B, 'ProjectError' e C)
--     => Either e String
-- three = do
--     'Left' ('putError' A)
--     'Left' ('putError' B)
--     'Left' ('putError' C)
--     'pure' "hello, world"
-- @
--
-- This is pretty silly, sure! But if you have a function which can throw
-- multiple error types, this is what you'll end up with. Let's suppose you
-- know how to handle the error @B@, but no others. You want this to show
-- up in the types of your functions - no more @B@s, but everything else is
-- still on the loose! First, we'll specialize the type very slightly:
--
-- @
-- three'
--     :: ('ProjectError' e A, 'ProjectError' e C)
--     => Either (Either B e) String
-- three' = three
-- @
--
-- Now, we can pattern match directly on the @B@ in the 'Left'. Just like
-- with the base case, we can skip manually providing the specializing type
-- signature:
--
-- @
-- main :: IO ()
-- main = do
--     case three of
--         Left err ->
--             case err of
--                 Left B -> print "It was B."
--                 Right other -> error "wasn't B!"
--         Right msg ->
--             putStrLn msg
-- @
--
-- This pattern is a little boring, and we'll introduce some helper
-- functions to match and fix it directly.
--
-- @since 0.0.0.0
instance {-# OVERLAPPING #-} ProjectError (Either a b) a where
    putError = Left

-- | This is the "recursive case" in our plucking experiment. If you have
-- a @single@ value, and it's not in the 'Left' side of the 'Either', but
-- it *can* be extracted from the 'Right' of an 'Either', then we recurse:
-- we call 'putError' and stuff it in the right hand side.
--
-- Let's review the example from above, @three@.
--
-- @
-- three
--     :: ('ProjectError' e A, 'ProjectError' e B, 'ProjectError' e C)
--     => Either e String
-- three = do
--     'Left' ('putError' A)
--     'Left' ('putError' B)
--     'Left' ('putError' C)
--     'pure' "hello, world"
-- @
--
-- How does this exactly work out?
-- Well, let's pick the @A@ type to pull out first.
--
-- @
-- three
--     :: ('ProjectError' e B, 'ProjectError' e C)
--     => Either (Either A e) String
-- three = do
--     'Left' ('putError' A)
--     'Left' ('putError' B)
--     'Left' ('putError' C)
--     'pure' "hello, world"
-- @
--
-- The "terminating" instance (@instance 'ProjectError' ('Either' a b) a@)
-- is the one that comes into play here. We can hand-specialize the
-- implementation by copying in the definition of 'putError' for that type:
--
-- @
-- three
--     :: ('ProjectError' e B, 'ProjectError' e C)
--     => Either (Either A e) String
-- three = do
--     'Left' ('Left' A)
--     'Left' ('putError' B)
--     'Left' ('putError' C)
--     'pure' "hello, world"
-- @
--
-- Neat! Now, we can also partially specialize the @B@ and @C@ cases to
-- this instance:
--
-- @
-- three
--     :: ('ProjectError' e B, 'ProjectError' e C)
--     => Either (Either A e) String
-- three = do
--     'Left' ('Left' A)
--     'Left' ('Right' ('putError' B))
--     'Left' ('Right' ('putError' C))
--     'pure' "hello, world"
-- @
--
-- We can specialize the type of @e@ again, this time plucking out the @C@
-- constraint.
--
-- @
-- three
--     :: ('ProjectError' e B)
--     => Either (Either A (Either C e)) String
-- @
--
-- I specifically chose @C@ because *ordering does not matter* and this is
-- cool. So now we can specialize our implementation again. Let's zoom in
-- on the @C@ case:
--
-- @
--     'putError' C
--          :: Either C x
-- @
--
-- This specializes to 'Left', so we can plug that in:
--
-- @
--     'Left' ('Right' ('Left' C))
-- @
--
-- (if you feel tempted to march, please take a small break from reading
-- these documentations).
--
-- Likewise, we can further specialize the @'putError' B@ to make the type
-- class magic explicit. Because we're delegating into the final type
-- parameter, we're using the recursive instance, which is @'Right' . 'putError'@:
--
-- @
--     'Left' ('Right' ('Right' ('putError' B)))
-- @
--
-- Stitching that all together, we get:
--
-- @
-- three
--     :: ('ProjectError' e B)
--     => Either (Either A (Either C e)) String
-- three = do
--     'Left' ('Left' A)
--     'Left' ('Right' ('Right' ('putError' B)))
--     'Left' ('Right' ('Left' C))
--     'pure' "hello, world"
-- @
--
-- There's one final expansion we can do here. Because there is only
-- a single constraint remaining, we can use the "identity" base case.
-- In that case, @'putError' = 'id'@, so we can finish off the polymorphic
-- type class magic:
--
-- @
-- three
--     :: Either (Either A (Either C B)) String
-- three = do
--     'Left' ('Left' A)
--     'Left' ('Right' ('Right' B))
--     'Left' ('Right' ('Left' C))
--     'pure' "hello, world"
-- @
--
-- As with every other example, all of this "unwinding" is totally
-- unnecessary for you, the library user, to ever do. You can pattern match
-- directly on the totally polymorphic type to match on exceptions, and the
-- errors you match on determine how GHC constructs the values for you.
--
-- In practice, you'll use other functions to manipulate these exceptions.
--
-- @since 0.0.0.0
instance
    {-# OVERLAPPABLE #-}
    (ProjectError b c)
  => ProjectError (Either a b) c
  where
    putError = Right . putError

-- | Finally, we have an "error" instance. This one is here to blow up in
-- the event that you try to trick GHC into thinking you've handled all the
-- possible errors, when you've only really handled one. You shouldn't see
-- this.
--
-- @since 0.0.0.0
instance {-# OVERLAPPABLE #-} (TypeError ('Text "No (remind me to write a better error message)")) => ProjectError a b where
    putError = undefined

-- | It's often annoying to write a bunch of 'ProjectError' annotations.
-- Consider the @three@ example:
--
-- @
-- three
--     :: ('ProjectError' e A, 'ProjectError' e B, 'ProjectError' e C)
--     => Either e String
-- three = do
--     'Left' ('putError' A)
--     'Left' ('putError' B)
--     'Left' ('putError' C)
--     'pure' "hello, world"
-- @
--
-- That constraint is so annoying and repetitive. So instead this is
-- defined to allow you to write:
--
-- @
-- three :: 'OneOf' e [A, B, C] => Either e String
-- @
--
-- @since 0.0.0.0
type family OneOf a as :: Constraint where
    OneOf a (x ': xs) = (ProjectError a x, OneOf a xs)
    OneOf a '[] = ()

-- | Use this to 'throw' an error in 'Either' and also promote it into some
-- big polymorphic type.
--
-- @
-- three
--     :: ('OneOf' e [A, B, C])
--     => Either e String
-- three = do
--     'throw' A
--     'throw' B
--     'throw' C
--     'pure' "hello, world"
-- @
--
-- For 'ExceptT', see the variant 'throwT'.
--
-- @since 0.0.0.0
throw :: ( ProjectError e' e, MonadError e' m ) => e -> m x
throw = throwError . putError

-- | This function is useful to handle a single exception type among many.
-- You pick which error you're going to handle by the handler function's
-- input, and it magically rearranges everything else to 'just work' for
-- you.
--
-- Let's say we have a program that throws three errors:
--
-- @
-- data A = A
--
-- three :: ('OneOf' error [A, B, C]) => Either error String
-- @
--
-- At this point in our program logic, we can handle an @A@ just fine, but
-- we don't know how to deal with a @B@ or a @C@ just yet. So we'll use
-- 'catchOne', and in the handler function, we'll pattern match on @A@.
--
-- @
-- handleThree :: ('OneOf' error [B, C]) => Either error String
-- handleThree =
--     'catchOne' three $ \\A -> pure "It was an A!"
-- @
--
-- If you want to rethrow the error with a different kind of exception, you
-- can totally do that too.
--
-- @
-- data Z = Z
--
-- aToZ :: ('OneOf' error [B, C, Z]) => Either error String
-- aToZ =
--      'catchOne' three $ \\A -> 'throw' Z
-- @
--
-- @since 0.0.0.0
catchOne :: Either (Either a b) c -> (a -> Either b c) -> Either b c
catchOne e k = catch e $ \case
    Left a ->
        k a
    Right b ->
        Left b

-- | This function can be used to handle the entirety of the error cases.
-- Using this will require that you pattern match on the error, which can
-- allow you to handle multiple cases.
--
-- Note that each different error case will live in a different "level" of
-- the 'Either' that it's contained in. So you'll need to nest the pattern
-- matching.
--
-- To give an example, let's consider the case presented in 'catchOne'
-- - a function that throws three errors.
--
-- @
-- data A = A
--
-- three :: ('OneOf' error [A, B, C]) => Either error String
-- @
--
-- We can handle the possibility of @A@ and @B@ at the same time:
--
-- @
-- handlingTwoErrors :: 'OneOf' error [C] => 'Either' error 'String'
-- handlingTwoErrors =
--     catch three $ \\ error -> case error of
--          'Left' A ->
--              'pure' "It's an A"
--          'Right' ('Left' B) ->
--              'pure' "It's a B"
--          'Right' ('Right' other) ->
--              'rethrow' other
-- @
--
-- You will probably have an easier time using 'catchOne' sequentially,
-- which would look like this:
--
-- @
-- handlingTwoErrors :: 'OneOf' error [C] => 'Either' error 'String'
-- handlingTwoErrors =
--     'catchOne'
--         ('catchOne' three $ \\A -> 'pure' "It's an A")
--         $ \\B -> 'pure' "It's a B!"
-- @
--
-- TODO: Should this be the function that 'catch' refers to? Or should
-- 'catchOne' be the guided case, and this is @catchAll@?
--
-- @since 0.0.0.0
catch :: MonadError a' m => Either a b -> (a -> m b) -> m b
catch e k = case e of
    Left a  ->
        k a
    Right b ->
        return b

-- | When you're pattern matching on an unknown error, and you don't know
-- how to handle it, you'll probably want to 'rethrow' it.
--
-- @
-- handlingThree =
--      'catch' three $ \\error ->
--          case error of
--              'Left' A ->
--                  pure "It was an A."
--              'Right' other ->
--                  'rethrow' other
-- @
--
-- In practice, you'll probably be using 'catchOne', and this won't be
-- necessary.
--
-- @since 0.0.0.0
rethrow :: MonadError e m => e -> m a
rethrow = throwError

-- | Like 'catch', but promoted to the 'ExceptT' monad transformer.
--
-- @since 0.0.0.0
catchT
    :: (MonadError e' m)
    => ExceptT e m a
    -> (e -> m a)
    -> m a
catchT action handler = do
    ea <- runExceptT action
    case ea of
        Left err ->
            handler err
        Right a ->
            pure a


-- | Like 'catchOne', but promoted to the 'ExceptT' monad transformer.
--
-- @since 0.0.0.1
catchOneT
    :: (MonadError e' m)
    => ExceptT (Either e e') m a
    -> (e -> m a)
    -> m a
catchOneT action handler = do
    ea <- runExceptT action
    case ea of
        Left (Left err) ->
            handler err
        Left (Right err) ->
            throwError err
        Right a ->
            pure a

-- | Like 'throw', but promoted to the 'ExceptT' monad transformer.
--
-- @since 0.0.0.0
throwT :: (MonadError e' m, ProjectError e' e) => e -> m a
throwT = throw

-- | Like 'rethrow', but promoted to the 'ExceptT' monad transformer.
--
-- @since 0.0.0.1
rethrowT :: (Monad m) => e -> ExceptT e m a
rethrowT = throwE
