module Ide3.Module.Query where

import Ide3.Types.Internal
import Ide3.NewMonad
import Ide3.Query
{-
-- | Test if a module imports another
importsModule :: Module -> Symbol -> Bool
importsModule = moduleImportsModule
-}
importsModule' :: ( ModuleImportClass m
                        )
                     => ProjectInfo 
                     -> ModuleInfo 
                     -> Symbol 
                     -> SolutionResult m u Bool
importsModule' = moduleImportsModule'
{-
-- | Within the context of a project, find all of the symbols this module exports
--  This requires the project context as modules may export other modules,
--      necessitating finding what symbols they export, and so on
exportedSymbols :: (ProjectModuleClass m, ProjectExternModuleClass m)
                => ProjectInfo 
                -> Module 
                -> SolutionResult m u [ModuleChild Symbol]
exportedSymbols = moduleExportedSymbols
-}
exportedSymbols' :: ( ProjectModuleClass m
                    , ProjectExternModuleClass m 
                    , ModuleExportClass m 
                    , ModuleDeclarationClass m 
                    , ModuleImportClass m 
                    , ExternModuleExportClass m 
                    ) 
                 => ProjectInfo 
                 -> ModuleInfo 
                 -> SolutionResult m u [ModuleChild Symbol] 
exportedSymbols' = moduleExportedSymbols'
{-
-- | Within the context of a project, find all of the symbosl being imported by
-- a module
importedSymbols :: (ProjectModuleClass m, ProjectExternModuleClass m)
                => ProjectInfo 
                -> Module 
                -> SolutionResult m u [Symbol]
importedSymbols = moduleImportedSymbols
-}
importedSymbols' :: ( ProjectModuleClass m
                          , ProjectExternModuleClass m
                          , ModuleImportClass m
                          , ModuleExportClass m
                          , ModuleDeclarationClass m
                          , ExternModuleExportClass m
                          )
                => ProjectInfo 
                -> ModuleInfo
                -> SolutionResult m u [Symbol]
importedSymbols' = moduleImportedSymbols'
{-
-- | Within the context of a project, find all of the symbols which are visible
--  at the top level of this module 
internalSymbols :: (ProjectModuleClass m, ProjectExternModuleClass m) 
                => ProjectInfo 
                -> Module 
                -> SolutionResult m u [Symbol]
internalSymbols = moduleInternalSymbols
-}
internalSymbols' :: ( ProjectModuleClass m
                          , ProjectExternModuleClass m
                          , ModuleDeclarationClass m
                          , ModuleImportClass m
                          , ModuleExportClass m
                          , ExternModuleExportClass m
                          )
                => ProjectInfo 
                -> ModuleInfo
                -> SolutionResult m u [Symbol]
internalSymbols' = moduleInternalSymbols'
{-
-- | Given a sub-symbol, (such as a data constructor or a class method), find
--  the parent symbol and its siblings
--  If successful, the list will contain the parent symbol as its head, and the
--      siblings as the tail. The symbol provided will not be an item in the list
--  If the symbol is imported, it will be tagged as such
symbolTree :: (ProjectModuleClass m, ProjectExternModuleClass m)
           => ProjectInfo
           -> Module 
           -> Symbol 
           -> SolutionResult m u [ModuleChild Symbol]
symbolTree = moduleSymbolTree
-}
symbolTree' :: ( ProjectModuleClass m
                     , ProjectExternModuleClass m
                     , ModuleImportClass m
                     , ModuleDeclarationClass m
                     , ModuleExportClass m
                     , ExternModuleExportClass m
                     )
           => ProjectInfo
           -> ModuleInfo
           -> Symbol 
           -> SolutionResult m u [ModuleChild Symbol]
symbolTree' = moduleSymbolTree' 
