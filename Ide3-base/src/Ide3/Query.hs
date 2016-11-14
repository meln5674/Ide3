{-|
Module      : Ide3.Query
Description : Queries on solutions
Copyright   : (c) Andrew Melnick, 2016

License     : BSD3
Maintainer  : meln5674@kettering.edu
Stability   : experimental
Portability : POSIX

The functions in this module work with any instance of the NewMonad typeclasses,
and perform queries such as determining the visiblility of symbols.

This module is internal to the package, the individual functions are exported
under different names by the Ide3.*.Query modules. The functions must be
declared in the same module as they exhibit a high degree of mutual recursion,
and haskell does not like mutually recursive modules.

-}

{-# LANGUAGE LambdaCase #-}
module Ide3.Query where

import Data.Monoid
import Data.List

import Control.Monad
import Control.Monad.Trans.Except

import Ide3.Types.Internal
import Ide3.NewMonad

import qualified Ide3.Module.Extern as ExternModule
import qualified Ide3.Module.Internal as Module

import qualified Ide3.Import.Internal as Import
import qualified Ide3.Declaration as Declaration

-- | Find the symbols that would be imported into module using a whitelist
-- import
importWhitelistTree' :: ( ProjectModuleClass m
                        , ProjectExternModuleClass m
                        , ExternModuleExportClass m
                        , ModuleImportClass m
                        , ModuleExportClass m
                        , ModuleDeclarationClass m
                        )
              => ProjectInfo
              -> ModuleInfo -- ^ Info for module symbols are being imported from
              -> ImportKind -- ^ Specific import to search for
              -> SolutionResult u m [Symbol]
importWhitelistTree' pji mi i = do
    exSyms <- liftM (map getChild) $ eitherModuleExportedSymbols' pji mi
    case i of
        NameImport s
            | s `elem` exSyms -> return [s]
            | otherwise -> throwE 
                         $ SymbolNotExported mi s "importWhitelistTree'"
        AbsImport _ _ -> error "FOUND AN ABS IMPORT"
        AggregateImport s Nothing -> liftM (map getChild) 
                                         $ eitherModuleSymbolTree' pji mi s
        AggregateImport s (Just ss) -> do
            ls <- eitherModuleSymbolTree' pji mi s
            let ls' = map getChild ls
            case find (not . (`elem` ls')) ss of
                Just s' -> throwE $ NotSubSymbol s s' "importWhitelistTree'"
                Nothing -> return (s:ss)


-- | Find the symbols that would be imported into a module from a blacklist
-- import
importBlacklistTree' :: ( ProjectModuleClass m
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
importBlacklistTree' pji mi i = do
    whitelistSyms <- importWhitelistTree' pji mi i
    allSyms <- liftM (map getChild) 
                   $ eitherModuleExportedSymbols' pji mi
    return $ filter (not . (`elem` whitelistSyms)) allSyms

-- | Get the symbols provided by an import, ignoring qualification
importUnqualSymbolsProvided' :: ( ProjectModuleClass m
                                , ProjectExternModuleClass m
                                , ModuleImportClass m
                                , ModuleExportClass m
                                , ModuleDeclarationClass m
                                , ExternModuleExportClass m
                                )
                            => ProjectInfo 
                            -> Import 
                            -> SolutionResult u m [Symbol]
importUnqualSymbolsProvided' pji i = case i of
    (ModuleImport sym _ _) -> liftM (map getChild) 
                            $ eitherModuleExportedSymbols' pji (ModuleInfo sym)
    (WhitelistImport sym _ _ specs) -> do
        symbolsFromEach <- forM specs
                              $ (importWhitelistTree' pji (ModuleInfo sym)) 
        return $ concat symbolsFromEach
    (BlacklistImport sym _ _ specs) -> do
        symbolsFromEach <- forM specs
                         $ (importBlacklistTree' pji (ModuleInfo sym))
        return $ concat symbolsFromEach

-- | Get the symbols provided by an import
importSymbolsProvided' :: ( ProjectModuleClass m
                          , ProjectExternModuleClass m
                          , ModuleImportClass m
                          , ModuleExportClass m
                          , ModuleDeclarationClass m
                          , ExternModuleExportClass m
                          )
                      => ProjectInfo 
                      -> Import 
                      -> SolutionResult u m [Symbol]
importSymbolsProvided' pji i = Import.qualifySymbols
    qualification
    shouldQualify
    <$> importUnqualSymbolsProvided' pji i
  where
    qualification = Import.importedModuleName i
    shouldQualify = Import.isQualified i


-- | Test if an import provides a symbol
importProvidesSymbol' :: ( ProjectModuleClass m
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
importProvidesSymbol' pji i s = do
    syms <- importSymbolsProvided' pji i
    return $ s `elem` syms

-- | If an import provides a symbol, get all of the other symbols it provides
importOtherSymbols' :: ( ProjectModuleClass m
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
importOtherSymbols' pji i s = do
    p <- importProvidesSymbol' pji i s
    if p
        then do
            syms <- importSymbolsProvided' pji i
            return $ Just $ delete s syms
        else
            return Nothing

-- | Given a sub-symbol (class method, data constructor, etc...), find the other
-- sub-symbols and the parent symbol from this import
-- See 'moduleSymbolTree'
importSymbolTree' :: ( ProjectModuleClass m
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
importSymbolTree' pji i s = do
    liftM (map getChild)
        $ eitherModuleSymbolTree' pji (ModuleInfo $ Import.moduleName i) s

-- | Get the symbols an export provides
exportSymbolsProvided' :: ( ProjectModuleClass m
                          , ProjectExternModuleClass m
                          , ModuleExportClass m
                          , ModuleImportClass m
                          , ModuleDeclarationClass m
                          , ExternModuleExportClass m
                          )
                => ProjectInfo 
                -> ModuleInfo
                -> Export 
                -> SolutionResult u m [Symbol]
exportSymbolsProvided' pji mi e = case e of
    SingleExport s -> do
        internalSyms <- moduleInternalSymbols' pji mi
        if s `elem` internalSyms
            then return [s]
            else throwE $ SymbolNotFound mi s "exportSymbolsProvided"
    ModuleExport n
        | mi == ModuleInfo n -> moduleDeclaredSymbols pji mi
        | otherwise -> do
            valid <- moduleImportsModule' pji mi n
            if valid
                then do
                    syms <- moduleExportedSymbols' pji $ ModuleInfo n
                    return $ map getChild syms
                else throwE $ ModuleNotImported pji mi 
                                                (ModuleInfo n) 
                                                "exportSymbolsProvided"
    
    AggregateExport s (Just ss) -> do
        allSyms <- moduleDeclaredSymbols pji mi
        if exportedSyms `areAll` (`elem` allSyms)
            then do
                tree <- map getChild <$> moduleSymbolTree' pji mi s 
                case find (not . (`elem` tree)) ss of
                        Just s' -> throwE 
                                   $ NotSubSymbol s s' "exportSymbolsProvided"
                        Nothing -> return exportedSyms
            else throwE $ SymbolNotFound mi s "exportSymbolsProvided"
      where
        areAll = flip all
        exportedSyms = s:ss
    (AggregateExport s Nothing) -> do
        allSyms <- moduleDeclaredSymbols pji mi
        if s `elem` allSyms
            then liftM (map getChild) $ moduleSymbolTree' pji mi s
            else throwE $ SymbolNotFound mi s "exportSymbolsProvided"


-- | Test if a module imports another
moduleImportsModule' :: ( ModuleImportClass m
                        )
                     => ProjectInfo 
                     -> ModuleInfo 
                     -> Symbol 
                     -> SolutionResult u m Bool
moduleImportsModule' pji mi sym = do
    is <- liftM items $ getImports pji mi >>= mapM (getImport pji mi)
    return $ sym `elem` map Import.moduleName is

-- | Find all of the symbols a module exports
moduleExportedSymbols' :: ( ProjectModuleClass m
                          , ProjectExternModuleClass m
                          , ModuleExportClass m
                          , ModuleDeclarationClass m
                          , ModuleImportClass m
                          , ExternModuleExportClass m
                          )
                      => ProjectInfo 
                      -> ModuleInfo
                      -> SolutionResult u m [ModuleChild Symbol]
moduleExportedSymbols' pji mi = do
    localSyms <- moduleDeclaredSymbols pji mi
    exportSyms <- do
        getExports pji mi >>= \case
            Nothing -> return Nothing
            Just eis -> do
                es <- liftM items $ mapM (getExport pji mi) eis
                ss <- liftM concat $ mapM (exportSymbolsProvided' pji mi) es
                return $ Just $ map (ModuleChild mi) ss
    case exportSyms of
        Just ss -> return ss
        Nothing -> return $ map (ModuleChild mi) localSyms

-- | Find all symbols being imported by a module
moduleImportedSymbols' :: ( ProjectModuleClass m
                          , ProjectExternModuleClass m
                          , ModuleImportClass m
                          , ModuleExportClass m
                          , ModuleDeclarationClass m
                          , ExternModuleExportClass m
                          )
                => ProjectInfo 
                -> ModuleInfo
                -> SolutionResult u m [Symbol]
moduleImportedSymbols' pji mi = do
    imports <- liftM items $ getImports pji mi >>= mapM (getImport pji mi)
    liftM concat $ mapM (importSymbolsProvided' pji) imports

-- | Find the symbols visible at the top level of a module
moduleInternalSymbols' :: ( ProjectModuleClass m
                          , ProjectExternModuleClass m
                          , ModuleDeclarationClass m
                          , ModuleImportClass m
                          , ModuleExportClass m
                          , ExternModuleExportClass m
                          )
                => ProjectInfo 
                -> ModuleInfo
                -> SolutionResult u m [Symbol]
moduleInternalSymbols' pji mi = do
    localSyms <- moduleDeclaredSymbols pji mi
    importSyms <- moduleImportedSymbols' pji mi
    return $ importSyms ++ localSyms

-- | Get the symbols declared in a module
moduleDeclaredSymbols :: (ProjectModuleClass m, ModuleDeclarationClass m)
                      => ProjectInfo
                      -> ModuleInfo
                      -> SolutionResult u m [Symbol]
moduleDeclaredSymbols pji mi = do
    dis <- getDeclarations pji mi
    ds <- mapM (liftM item . getDeclaration pji mi) dis
    return $ concatMap Declaration.symbolsProvided ds
    
-- | Given a sub-symbol, (such as a data constructor or a class method), find
--  the parent symbol and its siblings
--  If successful, the list will contain the parent symbol as its head, and the
--      siblings as the tail. The symbol provided will not be an item in the
--      list
--  If the symbol is imported, it will be tagged with the module it imported
--      from
moduleSymbolTree' :: ( ProjectModuleClass m
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
moduleSymbolTree' pji mi sym = do
    declarations <- liftM items $ getDeclarations pji mi 
                                  >>= mapM (getDeclaration pji mi)
    let declSearchResult = Module.search (`Declaration.otherSymbols` sym) 
                                         declarations
    case declSearchResult of
        Just (_,syms) -> return $ map (ModuleChild mi) syms
        Nothing -> do
            imports <- liftM items $ getImports pji mi 
                                     >>= mapM (getImport pji mi) 
            let othersFromImport x = importOtherSymbols' pji x sym
            importSearchResult <- Module.searchM othersFromImport imports
            case importSearchResult of
                Just (i,_) -> do
                    otherSyms <- importSymbolTree' pji i sym
                    return $ map (ModuleChild mi) otherSyms
                Nothing -> throwE $ SymbolNotFound mi sym "moduleSymbolTree"

-- | Get the symbols exported by a module
getExternalSymbols :: ( ProjectModuleClass m
                     , ProjectExternModuleClass m
                     , ModuleImportClass m
                     , ModuleDeclarationClass m
                     , ModuleExportClass m
                     , ExternModuleExportClass m
                     )
                   => ProjectInfo 
                   -> ModuleInfo 
                   -> SolutionResult u m  [Symbol]
getExternalSymbols pji mi = liftM (map getChild) 
                            $ eitherModuleExportedSymbols' pji mi

-- | Test if a project has a module
hasModule :: (ProjectModuleClass m) 
          => ProjectInfo 
          -> ModuleInfo 
          -> SolutionResult u m Bool
hasModule pji mi = liftM (mi `elem`) $ getModules pji

-- | Test if a project has an external module
hasExternModule :: (ProjectExternModuleClass m)
                => ProjectInfo
                -> ModuleInfo
                -> SolutionResult u m Bool
hasExternModule pji mi = liftM (mi `elem`) $ getExternModules pji

-- | Get the symbols exported by either a local or external module
eitherModuleExportedSymbols' :: ( ProjectModuleClass m
                                , ProjectExternModuleClass m
                                , ExternModuleExportClass m
                                , ModuleExportClass m
                                , ModuleDeclarationClass m
                                , ModuleImportClass m
                                )
                => ProjectInfo
                -> ModuleInfo
                -> SolutionResult u m [ModuleChild Symbol]
eitherModuleExportedSymbols' pji mi = do
    hasEM <- hasExternModule pji mi
    if hasEM
        then externModuleExportedSymbols' pji mi
        else moduleExportedSymbols' pji mi

-- | Get the symbols exported by an external module
externModuleExportedSymbols' :: ( ProjectExternModuleClass m
                                , ExternModuleExportClass m
                                )
                            => ProjectInfo
                            -> ModuleInfo
                            -> SolutionResult u m [ModuleChild Symbol]
externModuleExportedSymbols' pji mi = do
    es <- getExternExports pji mi >>= mapM (getExternExport pji mi)
    return $ map (ModuleChild mi) $ concatMap ExternModule.exportSymbols es

-- | Given a symbol such as a class method of data constructor, find the rest
-- of the related symbols exported by a local or external module.
-- The result list's head is the data type/class, and the tail as the other
-- symbols
-- This does not return the symbol provided
eitherModuleSymbolTree' :: ( ProjectModuleClass m
                           , ProjectExternModuleClass m
                           , ExternModuleExportClass m
                           , ModuleImportClass m
                           , ModuleDeclarationClass m
                           , ModuleExportClass m
                           )
           => ProjectInfo
           -> ModuleInfo
           -> Symbol 
           -> SolutionResult u m [ModuleChild Symbol]
eitherModuleSymbolTree' pji mi s = do
    hasEM <- hasExternModule pji mi
    if hasEM
        then externModuleSymbolTree' pji mi s
        else moduleSymbolTree' pji mi s


-- | Given a symbol such as a class method of data constructor, find the rest
-- of the related symbols exported by an external module.
-- The result list's head is the data type/class, and the tail as the other
-- symbols
-- This does not return the symbol provided
externModuleSymbolTree' :: ( ExternModuleExportClass m 
                           )
                        => ProjectInfo
                        -> ModuleInfo 
                        -> Symbol 
                        -> SolutionResult u m [ModuleChild Symbol]
externModuleSymbolTree' pji mi s = do
    es <- getExternExports pji mi >>= mapM (getExternExport pji mi)
    case getFirst $ mconcat $ map look es of
        Just ss -> return $ map (ModuleChild mi) ss
        Nothing -> throwE $ SymbolNotExported mi s "Extern.symbolTree"
  where
    look (SingleExternExport s') | s == s' = First $ Just []
    look (MultiExternExport s' ss) | s' `elem` ss = First $ Just $ delete s' ss
    look _ = First Nothing

-- | Find all of the imports which import one module into another
moduleImportedByInModule :: (SolutionMonad m)
                         => ProjectInfo
                         -> ModuleInfo
                         -> ProjectInfo
                         -> ModuleInfo
                         -> SolutionResult u m [ImportId]
moduleImportedByInModule pji mi@(ModuleInfo sym) pji' mi' = do
    iis <- getImports pji' mi'
    flip filterM iis $ \ii -> do
        i <- getImport pji mi ii
        return $ Import.moduleName (item i) == sym
moduleImportedByInModule _ _ _ _ = return []

-- | Find all of the modules in a project which import another module
moduleImportedByInProject :: (SolutionMonad m)
                          => ProjectInfo
                          -> ModuleInfo
                          -> ProjectInfo
                          -> SolutionResult u m [ModuleChild [ImportId]]
moduleImportedByInProject pji mi pji' = do
    mis <- getModules pji'
    forM mis $ \mi' -> do
        iis <- moduleImportedByInModule pji mi pji' mi' 
        return $ ModuleChild mi iis

-- | Find all modules which import another module
moduleImportedBy :: (SolutionMonad m)
                 => ProjectInfo
                 -> ModuleInfo
                 -> SolutionResult u m [ProjectChild [ModuleChild [ImportId]]]
moduleImportedBy pji mi = do
    pjis <- getProjects
    forM pjis $ \pji' -> do
        mis <- moduleImportedByInProject pji mi pji'
        return $ ProjectChild pji' mis

-- | Search a module for declarations which provide a symbol    
moduleFindSymbol :: (SolutionMonad m)
           => ProjectInfo
           -> ModuleInfo
           -> Symbol
           -> SolutionResult u m [DeclarationInfo]
moduleFindSymbol pji mi sym = do
    dis <- getDeclarations pji mi
    flip filterM dis $ \di -> do
        d <- liftM item $ getDeclaration pji mi di
        return $ d `Declaration.providesSymbol` sym

-- | Search a project for declarations which provide a symbol
projectFindSymbol :: (SolutionMonad m)
                  => ProjectInfo
                  -> Symbol
                  -> SolutionResult u m [ModuleChild [DeclarationInfo]]
projectFindSymbol pji sym = do
    mis <- getModules pji
    results <- forM mis $ \mi -> 
        liftM (ModuleChild mi) $ moduleFindSymbol pji mi sym
    return $ filter (not . null . getChild) results

-- | Search the entire solution for declarations which provide a symbol
solutionFindSymbol :: (SolutionMonad m)
                   => Symbol
                   -> SolutionResult u m [ProjectChild 
                                            [ModuleChild 
                                                [DeclarationInfo]
                                            ]
                                         ]
solutionFindSymbol sym = do
    pjis <- getProjects
    results <- forM pjis $ \pji -> do
        liftM (ProjectChild pji) $ projectFindSymbol pji sym
    return $ filter (not . null . getChild) results
