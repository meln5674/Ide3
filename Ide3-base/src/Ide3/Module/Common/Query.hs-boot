module Ide3.Module.Common.Query where

import Ide3.Types
import Ide3.NewMonad

import Ide3.Module.Common.Types

exportedSymbols :: Monad m 
                => ProjectInfo
                -> EitherModule 
                -> SolutionResult m u [ModuleChild Symbol]

symbolTree :: Monad m 
           => ProjectInfo
           -> EitherModule 
           -> Symbol 
           -> SolutionResult m u [ModuleChild Symbol]
