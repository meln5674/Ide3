module Ide3.Module.Common where

import Ide3.Types
import Ide3.Monad

type EitherModule = Either Module ExternModule

exportedSymbols :: SolutionM m 
                => ProjectInfo
                -> EitherModule 
                -> SolutionResult m u [ModuleChild Symbol]

symbolTree :: SolutionM m 
           => ProjectInfo
           -> EitherModule 
           -> Symbol 
           -> SolutionResult m u [ModuleChild Symbol]
