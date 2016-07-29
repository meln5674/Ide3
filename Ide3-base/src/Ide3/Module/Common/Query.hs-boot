module Ide3.Module.Common.Query where

import Ide3.Types
import Ide3.Monad

import Ide3.Module.Common.Types

exportedSymbols :: SolutionM m 
                => ProjectInfo
                -> EitherModule 
                -> SolutionResult m u [ModuleChild Symbol]

symbolTree :: SolutionM m 
           => ProjectInfo
           -> EitherModule 
           -> Symbol 
           -> SolutionResult m u [ModuleChild Symbol]
