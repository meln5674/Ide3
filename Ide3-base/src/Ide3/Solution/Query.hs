module Ide3.Solution.Query where

import Ide3.Types
import Ide3.NewMonad
import Ide3.Query

findSymbol :: (SolutionMonad m)
                   => Symbol
                   -> SolutionResult u m [ProjectChild [ModuleChild [DeclarationInfo]]]
findSymbol = solutionFindSymbol
