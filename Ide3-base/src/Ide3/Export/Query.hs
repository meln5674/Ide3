module Ide3.Export.Query where

import Ide3.Query
import Ide3.Types.Internal
import Ide3.NewMonad
{-
symbolsProvided :: (ProjectModuleClass m, ProjectExternModuleClass m)
                => ProjectInfo 
                -> Module 
                -> Export 
                -> SolutionResult m u [Symbol]
symbolsProvided = exportSymbolsProvided
-}
symbolsProvided' :: ( ProjectModuleClass m
                          , ProjectExternModuleClass m
                          , ModuleExportClass m
                          , ModuleImportClass m
                          , ModuleDeclarationClass m
                          , ExternModuleExportClass m
                          )
                => ProjectInfo 
                -> ModuleInfo
                -> Export 
                -> SolutionResult m u [Symbol]
symbolsProvided' = exportSymbolsProvided'
