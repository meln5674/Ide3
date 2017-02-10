{-|
Module      : ProjectInitializer
Description : Initializing projects
Copyright   : (c) Andrew Melnick, 2016

License     : BSD3
Maintainer  : meln5674@kettering.edu
Stability   : experimental
Portability : POSIX

A Initializer is an abstract data type which attempts to create a new solution
-}
{-# LANGUAGE RankNTypes #-}
module ProjectInitializer
    ( ProjectInitializerResult (..)
    , ProjectInitializer (..)
    , runProjectInitializer
    , runProjectInitializerWithInput
    , noProjectInitializer
    , mapProjectInitializer
    ) where

import Data.Text (Text)

import Control.Monad
import Control.Monad.Trans.Except

import Ide3.Types

import Args

-- | The result of initialization
data ProjectInitializerResult
    = ProjectInitializerSucceeded Text Text ProjectInfo
    | ProjectInitializerFailed Text Text

-- | The initializer abstract type. Use runInitializer or runInitializerWithInput
-- to execute the actions of an initializer
newtype ProjectInitializer a m = ProjectInitializer
    { runProjectInitializerInternal :: forall u . a -> SolutionResult u m ProjectInitializerResult }

-- | Run an initializer with a list of strings to parse into arguments
runProjectInitializerWithInput :: (Args a)
               => ProjectInitializer a m
               -> [String]
               -> Either String (SolutionResult u m ProjectInitializerResult)
runProjectInitializerWithInput initializer = fmap (runProjectInitializerInternal initializer) . getArgsFrom

-- | Run an initializer with its arguments
runProjectInitializer :: ProjectInitializer a m
                      -> a
                      -> SolutionResult u m ProjectInitializerResult
runProjectInitializer = runProjectInitializerInternal

-- | An Initializer that represents no initialization capability, and will
-- always result in an error
noProjectInitializer :: Monad m => ProjectInitializer a m
noProjectInitializer = ProjectInitializer $ \_ -> throwE $ Unsupported "No project initializer specified"

mapProjectInitializer :: (forall b . m b -> m' b) 
                      -> ProjectInitializer a m 
                      -> ProjectInitializer a m'
mapProjectInitializer f (ProjectInitializer b) = ProjectInitializer $ \x -> mapExceptT f $ b x
