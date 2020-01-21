{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

-- |
module Data.Either.Plucky where

import           Control.Monad.Except
import           GHC.TypeLits

class ProjectError s t where
    putError :: t -> s
    getError :: s -> Maybe t

instance {-# OVERLAPPABLE #-} (TypeError ('Text "No (remind me to write a better error message)")) => ProjectError a b where
    putError = undefined
    getError = undefined

instance {-# OVERLAPPABLE #-} ProjectError a a where
    putError = id
    getError = Just

instance {-# OVERLAPPING #-} ProjectError (Either a b) a where
    putError = Left
    getError i = case i of
       Left a -> Just a
       Right _ -> Nothing

instance {-# OVERLAPPABLE #-} (ProjectError b c) => ProjectError (Either a b) c where
    putError = Right . putError
    getError i = case i of
        Left _ -> Nothing
        Right b -> getError b

type family OneOf a as where
    OneOf a (x ': xs) = (ProjectError a x, OneOf a xs)
    OneOf a '[] = ()

throw :: ProjectError e' e => e -> Either e' x
throw = Left . putError

rethrow :: e -> Either e a
rethrow = Left

catch :: Either a b -> (a -> Either a' b) -> Either a' b
catch e k = case e of
    Left a  -> k a
    Right b -> Right b

catchOne :: Either (Either a b) c -> (a -> Either b c) -> Either b c
catchOne e k = catch e $ \case
    Left a ->
        k a
    Right b ->
        Left b

promote :: Monad m => Either e a -> ExceptT e m a
promote = either throwError pure

catchT
    :: (Monad m)
    => ExceptT e m a
    -> (e -> ExceptT e' m a)
    -> ExceptT e' m a
catchT action handler = ExceptT $ do
    ea <- runExceptT action
    case ea of
        Left err ->
            runExceptT $ handler err
        Right a ->
            pure (Right a)

throwT :: (Monad m, ProjectError e' e) => e -> ExceptT e' m a
throwT = ExceptT . pure . throw
