{-|
Module      : Ide3.Export.Query
Description : Querying projects for information on exports
Copyright   : (c) Andrew Melnick, 2016

License     : BSD3
Maintainer  : meln5674@kettering.edu
Stability   : experimental
Portability : POSIX
-}

module Ide3.Export.Query where

import Ide3.Query
import Ide3.Types.Internal
import Ide3.NewMonad

-- | Find what symbols an export would provide in a module in a project
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
