module Ide3.Mechanism.Internal where

import Ide3.Monad
import Ide3.Types

getExternalSymbols :: SolutionM m => ProjectInfo -> ModuleInfo -> SolutionResult m u [Symbol]
getAnyModule :: SolutionM m => ProjectInfo -> ModuleInfo -> SolutionResult m u (Either Module ExternModule)
