{-|
Module      : Ide3.Import.Query
Description : Querying projects for information on imports
Copyright   : (c) Andrew Melnick, 2016

License     : BSD3
Maintainer  : meln5674@kettering.edu
Stability   : experimental
Portability : POSIX
-}

module Ide3.Import.Query where

import Ide3.Module.Common
import Ide3.Types.Internal
import Ide3.Query
import Ide3.NewMonad

-- | Find the symbols to import from a module using a whitelist import
whitelistTree' :: ( ProjectModuleClass m
                  , ProjectExternModuleClass m
                  , ExternModuleExportClass m
                  , ModuleImportClass m
                  , ModuleExportClass m
                  , ModuleDeclarationClass m
                  )
               => ProjectInfo
               -> ModuleInfo  -- ^ Info for module symbols are being imported from
               -> ImportKind     -- ^ Specific import to search for
               -> SolutionResult u m [Symbol]
whitelistTree' = importWhitelistTree' 

-- | Find the symbols to import from a module using a blacklist import
blacklistTree' :: ( ProjectModuleClass m
                  , ProjectExternModuleClass m
                  , ModuleImportClass m
                  , ModuleExportClass m
                  , ModuleDeclarationClass m
                  , ExternModuleExportClass m
                  )
               => ProjectInfo
               -> ModuleInfo -- ^ Module symbols are being imported from
               -> ImportKind     -- ^ Import to blacklist
               -> SolutionResult u m [Symbol]
blacklistTree' = importBlacklistTree' 

-- | Get the symbols provided by an import, ignoring qualification
unqualSymbolsProvided' :: ( ProjectModuleClass m
                          , ProjectExternModuleClass m
                          , ModuleImportClass m
                          , ModuleExportClass m
                          , ModuleDeclarationClass m
                          , ExternModuleExportClass m
                          )
                       => ProjectInfo 
                       -> Import 
                       -> SolutionResult u m [Symbol]
unqualSymbolsProvided' = importUnqualSymbolsProvided'

-- | Get the symbols provided by an import
symbolsProvided' :: ( ProjectModuleClass m
                    , ProjectExternModuleClass m
                    , ModuleImportClass m
                    , ModuleExportClass m
                    , ModuleDeclarationClass m
                    , ExternModuleExportClass m
                    )
                 => ProjectInfo 
                 -> Import 
                 -> SolutionResult u m [Symbol]
symbolsProvided' = importSymbolsProvided' 

-- | Test if an import provides a symbol
providesSymbol' :: ( ProjectModuleClass m
                   , ProjectExternModuleClass m
                   , ModuleImportClass m
                   , ModuleExportClass m
                   , ModuleDeclarationClass m
                   , ExternModuleExportClass m
                   )
                => ProjectInfo 
                -> Import 
                -> Symbol 
                -> SolutionResult u m Bool
providesSymbol' = importProvidesSymbol' 

-- | If this import provides a symbol, get all of the other symbols it provides
otherSymbols' :: ( ProjectModuleClass m
                 , ProjectExternModuleClass m
                 , ModuleImportClass m
                 , ModuleExportClass m
                 , ModuleDeclarationClass m
                 , ExternModuleExportClass m
                 )                         
              => ProjectInfo 
              -> Import 
              -> Symbol 
              -> SolutionResult u m (Maybe [Symbol])
otherSymbols' = importOtherSymbols' 

-- | Given a sub-symbol (class method, data constructor, etc...), find the other
-- sub-symbols and the parent symbol from this import
-- See 'Ide3.Module.symbolTree'
symbolTree' :: ( ProjectModuleClass m
               , ProjectExternModuleClass m
               , ModuleImportClass m
               , ModuleExportClass m
               , ModuleDeclarationClass m
               , ExternModuleExportClass m
               )
            => ProjectInfo 
            -> Import 
            -> Symbol 
            -> SolutionResult u m [Symbol]
symbolTree' = importSymbolTree' 
