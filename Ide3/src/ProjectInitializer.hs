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
module ProjectInitializer
    ( ProjectInitializerResult (..)
    , ProjectInitializer (..)
    , runProjectInitializer
    , runProjectInitializerWithInput
    , noProjectInitializer
    ) where

import System.Exit
import System.Process
import System.Directory
import System.FilePath

import Data.List

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Except

import Ide3.Types

import Ide3.NewMonad
import Ide3.Digest

import Args
import Viewer

-- | The result of initialization
data ProjectInitializerResult
    = ProjectInitializerSucceeded String String
    | ProjectInitializerFailed String String

-- | The initializer abstract type. Use runInitializer or runInitializerWithInput
-- to execute the actions of an initializer
newtype ProjectInitializer a m u = ProjectInitializer
    { runProjectInitializerInternal :: a -> SolutionResult u m ProjectInitializerResult }

-- | Run an initializer with a list of strings to parse into arguments
runProjectInitializerWithInput :: (Monad m, Args a)
               => ProjectInitializer a m u 
               -> [String]
               -> Either String (SolutionResult u m ProjectInitializerResult)
runProjectInitializerWithInput initializer = liftM (runProjectInitializerInternal initializer) . getArgsFrom

-- | Run an initializer with its arguments
runProjectInitializer :: (Monad m, Args a)
               => ProjectInitializer a m u
               -> a
               -> SolutionResult u m ProjectInitializerResult
runProjectInitializer = runProjectInitializerInternal

-- | An Initializer that represents no initialization capability, and will
-- always result in an error
noProjectInitializer :: Monad m => ProjectInitializer a m u
noProjectInitializer = ProjectInitializer $ \_ -> throwE $ Unsupported "No project initializer specified"

