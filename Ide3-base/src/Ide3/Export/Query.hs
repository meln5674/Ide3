module Ide3.Export.Query where

import Ide3.Query
import Ide3.Types
import Ide3.NewMonad

symbolsProvided :: (ProjectModuleClass m, ProjectExternModuleClass m)
                => ProjectInfo 
                -> Module 
                -> Export 
                -> SolutionResult m u [Symbol]
symbolsProvided = exportSymbolsProvided
