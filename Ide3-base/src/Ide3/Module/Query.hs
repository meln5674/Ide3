{-|
Module      : Ide3.Module.Query
Description : Queries on modules
Copyright   : (c) Andrew Melnick, 2016

License     : BSD3
Maintainer  : meln5674@kettering.edu
Stability   : experimental
Portability : POSIX
-}
module Ide3.Module.Query where

import Ide3.Types.Internal
import Ide3.NewMonad
import Ide3.Query

-- | Test if a module imports another
importsModule' :: ( ModuleImportClass m
                        )
                     => ProjectInfo 
                     -> ModuleInfo 
                     -> Symbol 
                     -> SolutionResult u m Bool
importsModule' = moduleImportsModule'

-- | Within the context of a project, find all of the symbols this module exports
exportedSymbols' :: ( ProjectModuleClass m
                    , ProjectExternModuleClass m 
                    , ModuleExportClass m 
                    , ModuleDeclarationClass m 
                    , ModuleImportClass m 
                    , ExternModuleExportClass m 
                    ) 
                 => ProjectInfo 
                 -> ModuleInfo 
                 -> SolutionResult u m [ModuleChild Symbol] 
exportedSymbols' = moduleExportedSymbols'

-- | Within the context of a project, find all of the symbosl being imported by
-- a module
importedSymbols' :: ( ProjectModuleClass m
                          , ProjectExternModuleClass m
                          , ModuleImportClass m
                          , ModuleExportClass m
                          , ModuleDeclarationClass m
                          , ExternModuleExportClass m
                          )
                => ProjectInfo 
                -> ModuleInfo
                -> SolutionResult u m [Symbol]
importedSymbols' = moduleImportedSymbols'

-- | Within the context of a project, find all of the symbols which are visible
--  at the top level of this module 
internalSymbols' :: ( ProjectModuleClass m
                          , ProjectExternModuleClass m
                          , ModuleDeclarationClass m
                          , ModuleImportClass m
                          , ModuleExportClass m
                          , ExternModuleExportClass m
                          )
                => ProjectInfo 
                -> ModuleInfo
                -> SolutionResult u m [Symbol]
internalSymbols' = moduleInternalSymbols'

-- | Given a sub-symbol, (such as a data constructor or a class method), find
--  the parent symbol and its siblings
--  If successful, the list will contain the parent symbol as its head, and the
--      siblings as the tail. The symbol provided will not be an item in the list
--  If the symbol is imported, it will be tagged as such
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
           -> SolutionResult u m [ModuleChild Symbol]
symbolTree' = moduleSymbolTree' 

-- | Find all modules which import a given module
importedBy :: (SolutionMonad m)
           => ProjectInfo
           -> ModuleInfo
           -> SolutionResult u m [ProjectChild [ModuleChild [ImportId]]]
importedBy = moduleImportedBy

-- | Find all declarations in a module which provide a symbol
findSymbol :: (SolutionMonad m)
           => ProjectInfo
           -> ModuleInfo
           -> Symbol
           -> SolutionResult u m [DeclarationInfo]
findSymbol = moduleFindSymbol
