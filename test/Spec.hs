{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Main where

import Data.Either.Plucky
import           Control.Monad.Except

main :: IO ()
main = putStrLn "The test suite is whether or not it compiles."

data X = X
data Y = Y
data Z = Z

foo :: (ProjectError a X, ProjectError a Y) => Either a b
foo = do
    throw X
    throw Y

fooOf
    :: (e `OneOf` [X, Y, Z])
    => Either e String
fooOf = do
    throw X
    throw Z
    throw Y

fooOf'
    :: (e `OneOf` [Y, Z])
    => Either (Either X e) String
fooOf' = fooOf

bar :: (ProjectError a Y, ProjectError a Z) => Either a b
bar = catch foo $ \case
    Left X ->
        throw Z
    Right a ->
        rethrow a

bar' :: (ProjectError a Y, ProjectError a Z) => Either a b
bar' = catchOne foo $ \X -> throw Z

huh :: Either x String
huh = catch (throw X) $ \case
    X -> Right  "hello"

foo' :: (Monad m, ProjectError e X, ProjectError e Y) => ExceptT e m a
foo' = promote foo

foo'' :: (MonadError e m, ProjectError e X, ProjectError e Y) => m a
foo'' = either throwError pure foo

