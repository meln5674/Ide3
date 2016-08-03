module Ide3.Import.Query where

import Ide3.Module.Common
import Ide3.Types.Internal
import Ide3.Query
import Ide3.NewMonad
{-
-- | Find the symbols to import from a module using a whitelist import
whitelistTree :: (ProjectModuleClass m, ProjectExternModuleClass m)
              => ProjectInfo
              -> EitherModule  -- ^ Module symbols are being imported from
              -> ImportKind     -- ^ Specific import to search for
              -> SolutionResult m u [Symbol]
whitelistTree = importWhitelistTree
-}
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
              -> SolutionResult m u [Symbol]
whitelistTree' = importWhitelistTree' 
{-
-- | Find the symbosl to import from a module using a blacklist import
blacklistTree :: (ProjectModuleClass m, ProjectExternModuleClass m) 
              => ProjectInfo
              -> EitherModule   -- ^ Module symbols are being imported from
              -> ImportKind     -- ^ Import to blacklist
              -> SolutionResult m u [Symbol]
blacklistTree = importBlacklistTree
-}
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
              -> SolutionResult m u [Symbol]
blacklistTree' = importBlacklistTree' 
{-
-- | Get the symbols provided by an import, ignoring qualification
unqualSymbolsProvided :: (ProjectModuleClass m, ProjectExternModuleClass m) 
                      => ProjectInfo 
                      -> Import 
                      -> SolutionResult m u [Symbol]
unqualSymbolsProvided = importUnqualSymbolsProvided
-}
unqualSymbolsProvided' :: ( ProjectModuleClass m
                                , ProjectExternModuleClass m
                                , ModuleImportClass m
                                , ModuleExportClass m
                                , ModuleDeclarationClass m
                                , ExternModuleExportClass m
                                )
                            => ProjectInfo 
                            -> Import 
                            -> SolutionResult m u [Symbol]
unqualSymbolsProvided' = importUnqualSymbolsProvided'
{-
-- | Get the symbols provided by an import
symbolsProvided :: (ProjectModuleClass m, ProjectExternModuleClass m) 
                => ProjectInfo 
                -> Import 
                -> SolutionResult m u [Symbol]
symbolsProvided = importSymbolsProvided
-}
symbolsProvided' :: ( ProjectModuleClass m
                          , ProjectExternModuleClass m
                          , ModuleImportClass m
                          , ModuleExportClass m
                          , ModuleDeclarationClass m
                          , ExternModuleExportClass m
                          )
                      => ProjectInfo 
                      -> Import 
                      -> SolutionResult m u [Symbol]
symbolsProvided' = importSymbolsProvided' 
{-
-- | Test if an import provides a symbol
providesSymbol :: (ProjectModuleClass m, ProjectExternModuleClass m) 
               => ProjectInfo 
               -> Import 
               -> Symbol 
               -> SolutionResult m u Bool
providesSymbol = importProvidesSymbol
-}
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
                     -> SolutionResult m u Bool
providesSymbol' = importProvidesSymbol' 
{-
-- | If this import provides a symbol, get all of the other symbols it provides
otherSymbols :: (ProjectModuleClass m, ProjectExternModuleClass m) 
             => ProjectInfo 
             -> Import 
             -> Symbol 
             -> SolutionResult m u (Maybe [Symbol])
otherSymbols = importOtherSymbols
-}
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
                   -> SolutionResult m u (Maybe [Symbol])
otherSymbols' = importOtherSymbols' 
{-
-- | Given a sub-symbol (class method, data constructor, etc...), find the other
-- sub-symbols and the parent symbol from this import
-- See 'Ide3.Module.symbolTree'
symbolTree :: (ProjectModuleClass m, ProjectExternModuleClass m) 
           => ProjectInfo 
           -> Import 
           -> Symbol 
           -> SolutionResult m u [Symbol]
symbolTree = importSymbolTree
-}
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
                 -> SolutionResult m u [Symbol]
symbolTree' = importSymbolTree' 
