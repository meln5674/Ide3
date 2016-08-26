module Ide3.Project.Query where

import Ide3.Types
import Ide3.NewMonad
import Ide3.Query

findSymbol :: (SolutionMonad m)
                  => ProjectInfo
                  -> Symbol
                  -> SolutionResult u m [ModuleChild [DeclarationInfo]]
findSymbol = projectFindSymbol
