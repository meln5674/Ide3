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
{-# LANGUAGE TypeFamilies #-}
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

import Ide3.Types

import Args

-- | The result of initialization
data InitializerResult m
    = InitializerSucceeded String String (PersistToken m)
    | InitializerFailed String String

-- | The initializer abstract type. Use runInitializer or runInitializerWithInput
-- to execute the actions of an initializer
newtype Initializer a m = Initializer
    { runInitializerInternal :: forall u . a -> SolutionResult u m (InitializerResult m) }

-- | Run an initializer with a list of strings to parse into arguments
runInitializerWithInput :: (Args a) 
               => Initializer a m
               -> [String]
               -> Either String (SolutionResult u m (InitializerResult m) )
runInitializerWithInput initializer = liftM (runInitializerInternal initializer) . getArgsFrom

-- | Run an initializer with its arguments
runInitializer :: Initializer a m
               -> a
               -> SolutionResult u m (InitializerResult m)
runInitializer = runInitializerInternal


-- | An Initializer that represents no initialization capability, and will
-- always result in an error
noInitializer :: Monad m => Initializer a m
noInitializer = Initializer $ \_ -> throwE $ Unsupported "No initializer specified"

mapInitializer :: ( PersistToken m ~ PersistToken m'
                  , Functor m
                  )
               => (forall b . m b -> m' b) 
               -> Initializer a m 
               -> Initializer a m'
mapInitializer f (Initializer b) = Initializer $ \x -> mapExceptT f $ fmap repack $ b x
    
  where
    repack (InitializerSucceeded out err tok) = InitializerSucceeded out err tok
    repack (InitializerFailed out err) = InitializerFailed out err
    

