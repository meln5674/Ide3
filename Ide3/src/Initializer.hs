{-|
Module      : Initializer
Description : Initializing solutions
Copyright   : (c) Andrew Melnick, 2016

License     : BSD3
Maintainer  : meln5674@kettering.edu
Stability   : experimental
Portability : POSIX

A Initializer is an abstract data type which attempts to create a new solution
-}
{-# LANGUAGE RankNTypes #-}
module Initializer
    ( InitializerResult (..)
    , Initializer (..)
    , runInitializer
    , runInitializerWithInput
    , noInitializer
    , mapInitializer
    ) where


import Control.Monad
import Control.Monad.Trans.Except

import Ide3.NewMonad
import Ide3.Digest

import Ide3.Types

import Args
import Viewer

-- | The result of initialization
data InitializerResult
    = InitializerSucceeded String String
    | InitializerFailed String String

-- | The initializer abstract type. Use runInitializer or runInitializerWithInput
-- to execute the actions of an initializer
newtype Initializer a m = Initializer
    { runInitializerInternal :: forall u . a -> SolutionResult u m InitializerResult }

-- | Run an initializer with a list of strings to parse into arguments
runInitializerWithInput :: (Monad m, Args a) 
               => Initializer a m
               -> [String]
               -> Either String (SolutionResult u m InitializerResult)
runInitializerWithInput initializer = liftM (runInitializerInternal initializer) . getArgsFrom

-- | Run an initializer with its arguments
runInitializer :: (Monad m, Args a)
               => Initializer a m
               -> a
               -> SolutionResult u m InitializerResult
runInitializer = runInitializerInternal


-- | An Initializer that represents no initialization capability, and will
-- always result in an error
noInitializer :: Monad m => Initializer a m
noInitializer = Initializer $ \_ -> throwE $ Unsupported "No initializer specified"

mapInitializer :: (forall a . m a -> m' a) -> Initializer a m -> Initializer a m'
mapInitializer f (Initializer b) = Initializer $ \x -> mapExceptT f $ b x
    
