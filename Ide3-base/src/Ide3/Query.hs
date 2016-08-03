module Ide3.Query where

import Data.Monoid
import Data.List
import Data.Maybe
import qualified Data.Map as Map

import Control.Monad
import Control.Monad.Trans.Except

import Ide3.Types.Internal
import Ide3.NewMonad

import Ide3.Module.Common (EitherModule)
import qualified Ide3.Module.Extern as ExternModule
import qualified Ide3.Module.Common as EitherModule
import qualified Ide3.Module.Internal as Module

import qualified Ide3.Import.Internal as Import
import qualified Ide3.Declaration as Declaration

{-
-- | Find the symbols to import from a module using a whitelist import
importWhitelistTree :: (ProjectModuleClass m, ProjectExternModuleClass m)
              => ProjectInfo
              -> EitherModule  -- ^ Module symbols are being imported from
              -> ImportKind     -- ^ Specific import to search for
              -> SolutionResult m u [Symbol]
importWhitelistTree pji m i = do
    exSyms <- map getChild <$> eitherModuleExportedSymbols pji m
    case i of
        NameImport s
            | s `elem` exSyms -> return [s]
            | otherwise -> throwE 
                         $ SymbolNotExported 
                            (EitherModule.info m)
                            s 
                            "importWhitelistTree"
        AbsImport _ _ -> error "FOUND AN ABS IMPORT"
        AggregateImport s Nothing -> map getChild <$> eitherModuleSymbolTree pji m s
        AggregateImport s (Just ss) -> do
            ls <- eitherModuleSymbolTree pji m s
            let ls' = map getChild ls
            case find (not . (`elem` ls')) ss of
                Just s' -> throwE $ NotSubSymbol s s' "importWhitelistTree"
                Nothing -> return (s:ss)
-}
-- | Find the symbols to import from a module using a whitelist import
importWhitelistTree' :: ( ProjectModuleClass m
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
importWhitelistTree' pji mi i = do
    exSyms <- liftM (map getChild) $ eitherModuleExportedSymbols' pji mi
    case i of
        NameImport s
            | s `elem` exSyms -> return [s]
            | otherwise -> throwE $ SymbolNotExported mi s "importWhitelistTree'"
        AbsImport _ _ -> error "FOUND AN ABS IMPORT"
        AggregateImport s Nothing -> map getChild <$> eitherModuleSymbolTree' pji mi s
        AggregateImport s (Just ss) -> do
            ls <- eitherModuleSymbolTree' pji mi s
            let ls' = map getChild ls
            case find (not . (`elem` ls')) ss of
                Just s' -> throwE $ NotSubSymbol s s' "importWhitelistTree'"
                Nothing -> return (s:ss)

{-
-- | Find the symbosl to import from a module using a blacklist import
importBlacklistTree :: (ProjectModuleClass m, ProjectExternModuleClass m)
              => ProjectInfo
              -> EitherModule   -- ^ Module symbols are being imported from
              -> ImportKind     -- ^ Import to blacklist
              -> SolutionResult m u [Symbol]
importBlacklistTree pji m i = do
    whitelistSyms <- importWhitelistTree pji m i
    allSyms <- map getChild <$> eitherModuleExportedSymbols pji m
    return $ filter (not . (`elem` whitelistSyms)) allSyms
-}
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
              -> SolutionResult m u [Symbol]
importBlacklistTree' pji mi i = do
    whitelistSyms <- importWhitelistTree' pji mi i
    allSyms <- map getChild <$> eitherModuleExportedSymbols' pji mi
    return $ filter (not . (`elem` whitelistSyms)) allSyms
{-
- | Get the symbols provided by an import, ignoring qualification
importUnqualSymbolsProvided :: (ProjectModuleClass m, ProjectExternModuleClass m)
                            => ProjectInfo 
                            -> Import 
                            -> SolutionResult m u [Symbol]
importUnqualSymbolsProvided pji i = case i of
    (ModuleImport sym _ _) -> getExternalSymbols pji (ModuleInfo sym)
    (WhitelistImport sym _ _ specs) -> do
        module_ <- getAnyModule pji (ModuleInfo sym)
        symbolsFromEach <- mapM (importWhitelistTree pji module_) specs
        return $ concat symbolsFromEach
    (BlacklistImport sym _ _ specs) -> do
        module_ <- getAnyModule pji (ModuleInfo sym)
        symbolsFromEach <- mapM (importBlacklistTree pji module_) specs
        return $ concat symbolsFromEach
-}
importUnqualSymbolsProvided' :: ( ProjectModuleClass m
                                , ProjectExternModuleClass m
                                , ModuleImportClass m
                                , ModuleExportClass m
                                , ModuleDeclarationClass m
                                , ExternModuleExportClass m
                                )
                            => ProjectInfo 
                            -> Import 
                            -> SolutionResult m u [Symbol]
importUnqualSymbolsProvided' pji i = case i of
    (ModuleImport sym _ _) -> liftM (map getChild) $ eitherModuleExportedSymbols' pji (ModuleInfo sym)
    (WhitelistImport sym _ _ specs) -> do
        symbolsFromEach <- mapM (importWhitelistTree' pji (ModuleInfo sym)) specs
        return $ concat symbolsFromEach
    (BlacklistImport sym _ _ specs) -> do
        symbolsFromEach <- mapM (importBlacklistTree' pji (ModuleInfo sym)) specs
        return $ concat symbolsFromEach
{-
-- | Get the symbols provided by an import
importSymbolsProvided :: (ProjectModuleClass m, ProjectExternModuleClass m)
                      => ProjectInfo 
                      -> Import 
                      -> SolutionResult m u [Symbol]
importSymbolsProvided pji i = Import.qualifySymbols
    qualification
    shouldQualify
    <$> importUnqualSymbolsProvided pji i
  where
    qualification = Import.importedModuleName i
    shouldQualify = Import.isQualified i
-}
importSymbolsProvided' :: ( ProjectModuleClass m
                          , ProjectExternModuleClass m
                          , ModuleImportClass m
                          , ModuleExportClass m
                          , ModuleDeclarationClass m
                          , ExternModuleExportClass m
                          )
                      => ProjectInfo 
                      -> Import 
                      -> SolutionResult m u [Symbol]
importSymbolsProvided' pji i = Import.qualifySymbols
    qualification
    shouldQualify
    <$> importUnqualSymbolsProvided' pji i
  where
    qualification = Import.importedModuleName i
    shouldQualify = Import.isQualified i
{-
-- | Test if an import provides a symbol
importProvidesSymbol :: (ProjectModuleClass m, ProjectExternModuleClass m)
                     => ProjectInfo 
                     -> Import 
                     -> Symbol 
                     -> SolutionResult m u Bool
importProvidesSymbol pji i s = do
    syms <- importSymbolsProvided pji i
    return $ s `elem` syms
-}
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
                     -> SolutionResult m u Bool
importProvidesSymbol' pji i s = do
    syms <- importSymbolsProvided' pji i
    return $ s `elem` syms
{-
-- | If this import provides a symbol, get all of the other symbols it provides
importOtherSymbols :: (ProjectModuleClass m, ProjectExternModuleClass m)
                   => ProjectInfo 
                   -> Import 
                   -> Symbol 
                   -> SolutionResult m u (Maybe [Symbol])
importOtherSymbols pji i s = do
    p <- importProvidesSymbol pji i s
    if p
        then do
            syms <- importSymbolsProvided pji i
            return $ Just $ delete s syms
        else
            return Nothing
-}
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
                   -> SolutionResult m u (Maybe [Symbol])
importOtherSymbols' pji i s = do
    p <- importProvidesSymbol' pji i s
    if p
        then do
            syms <- importSymbolsProvided' pji i
            return $ Just $ delete s syms
        else
            return Nothing
{-
-- | Given a sub-symbol (class method, data constructor, etc...), find the other
-- sub-symbols and the parent symbol from this import
-- See 'Ide3.Module.symbolTree'
importSymbolTree :: (ProjectModuleClass m, ProjectExternModuleClass m)
                 => ProjectInfo 
                 -> Import 
                 -> Symbol 
                 -> SolutionResult m u [Symbol]
importSymbolTree pji i s = do
    module_ <- getAnyModule pji $ ModuleInfo $ Import.moduleName i
    map getChild <$> eitherModuleSymbolTree pji module_ s
-}
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
                 -> SolutionResult m u [Symbol]
importSymbolTree' pji i s = do
    map getChild <$> eitherModuleSymbolTree' pji (ModuleInfo $ Import.moduleName i) s

{-
-- | Get a list of the symbols this export provides
exportSymbolsProvided :: (ProjectModuleClass m, ProjectExternModuleClass m)
                => ProjectInfo 
                -> Module 
                -> Export 
                -> SolutionResult m u [Symbol]
exportSymbolsProvided pji m e = case e of
    SingleExport s -> do
        internalSyms <- moduleInternalSymbols pji m
        if s `elem` internalSyms
            then return [s]
            else throwE $ SymbolNotFound (Module.info m) s "exportSymbolsProvided"
    ModuleExport n
        | m `moduleImportsModule` n -> do
            subM <- getModule pji (ModuleInfo n)
            syms <- moduleExportedSymbols pji subM
            return $ map getChild syms
        | m `Module.infoMatches` ModuleInfo n -> return $ map getChild $ Module.allSymbols m
        | otherwise -> throwE 
                     $ ModuleNotImported pji (moduleInfo m) (ModuleInfo n) "exportSymbolsProvided"
    
    AggregateExport s (Just ss) -> 
        if exportedSyms `areAll` (`elem` allSyms)
            then do
                tree <- map getChild <$> moduleSymbolTree pji m s 
                case find (not . (`elem` tree)) ss of
                        Just s' -> throwE $ NotSubSymbol s s' "exportSymbolsProvided"
                        Nothing -> return exportedSyms
            else throwE $ SymbolNotFound (Module.info m) s "exportSymbolsProvided"
      where
        allSyms = map getChild $ Module.allSymbols m
        areAll = flip all
        exportedSyms = s:ss
    (AggregateExport s Nothing)
        | s `elem` map getChild (Module.allSymbols m)
          -> map getChild <$> moduleSymbolTree pji m s
        | otherwise
          -> throwE $ SymbolNotFound (Module.info m) s "exportSymbolsProvided"
-}

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
                -> SolutionResult m u [Symbol]
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
                else throwE 
                     $ ModuleNotImported pji mi (ModuleInfo n) "exportSymbolsProvided"
    
    AggregateExport s (Just ss) -> do
        allSyms <- moduleDeclaredSymbols pji mi
        if exportedSyms `areAll` (`elem` allSyms)
            then do
                tree <- map getChild <$> moduleSymbolTree' pji mi s 
                case find (not . (`elem` tree)) ss of
                        Just s' -> throwE $ NotSubSymbol s s' "exportSymbolsProvided"
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


{-
-- | Test if a module imports another
moduleImportsModule :: Module -> Symbol -> Bool
moduleImportsModule m sym = sym `elem` map Import.moduleName (items $ Map.elems $ moduleImports m)
-}
moduleImportsModule' :: ( ModuleImportClass m
                        )
                     => ProjectInfo 
                     -> ModuleInfo 
                     -> Symbol 
                     -> SolutionResult m u Bool
moduleImportsModule' pji mi sym = do
    is <- liftM items $ getImports pji mi >>= mapM (getImport pji mi)
    return $ sym `elem` map Import.moduleName is
{-
-- | Within the context of a project, find all of the symbols this module exports
--  This requires the project context as modules may export other modules,
--      necessitating finding what symbols they export, and so on
moduleExportedSymbols :: (ProjectModuleClass m, ProjectExternModuleClass m)
                      => ProjectInfo 
                      -> Module 
                      -> SolutionResult m u [ModuleChild Symbol]
moduleExportedSymbols pji m = maybe allSyms getSymsFromExports $ moduleExports m
  where
    allSyms = return $ Module.allSymbols m
    getSymsFromExports es = do
        syms <- concat <$> mapM (exportSymbolsProvided pji m . item) es
        return $ map (Module.qualify m) syms
-}

moduleExportedSymbols' :: ( ProjectModuleClass m
                          , ProjectExternModuleClass m
                          , ModuleExportClass m
                          , ModuleDeclarationClass m
                          , ModuleImportClass m
                          , ExternModuleExportClass m
                          )
                      => ProjectInfo 
                      -> ModuleInfo
                      -> SolutionResult m u [ModuleChild Symbol]
moduleExportedSymbols' pji mi = do
    localSyms <- moduleDeclaredSymbols pji mi
    exportSyms <- do
        eis <- getExports pji mi
        case eis of
            Nothing -> return Nothing
            Just eis -> do
                es <- liftM items $ mapM (getExport pji mi) eis
                ss <- liftM concat $ mapM (exportSymbolsProvided' pji mi) es
                return $ Just $ map (ModuleChild mi) ss
    case exportSyms of
        Just ss -> return ss
        Nothing -> return $ map (ModuleChild mi) localSyms
{-
-- | Within the context of a project, find all of the symbosl being imported by
-- a module
moduleImportedSymbols :: (ProjectModuleClass m, ProjectExternModuleClass m)
                => ProjectInfo 
                -> Module 
                -> SolutionResult m u [Symbol]
moduleImportedSymbols pji m = concat <$> mapM providedBy imports
  where
    imports = moduleImports m
    providedBy = importSymbolsProvided pji . item
-}
moduleImportedSymbols' :: ( ProjectModuleClass m
                          , ProjectExternModuleClass m
                          , ModuleImportClass m
                          , ModuleExportClass m
                          , ModuleDeclarationClass m
                          , ExternModuleExportClass m
                          )
                => ProjectInfo 
                -> ModuleInfo
                -> SolutionResult m u [Symbol]
moduleImportedSymbols' pji mi = do
    imports <- liftM items $ getImports pji mi >>= mapM (getImport pji mi)
    liftM concat $ mapM (importSymbolsProvided' pji) imports
{-
-- | Within the context of a project, find all of the symbols which are visible
--  at the top level of this module 
moduleInternalSymbols :: (ProjectModuleClass m, ProjectExternModuleClass m)
                => ProjectInfo 
                -> Module 
                -> SolutionResult m u [Symbol]
moduleInternalSymbols pji m = do
    let decls = moduleDeclarations m
    importSyms <- moduleImportedSymbols pji m
    return $ importSyms ++ concatMap (Declaration.symbolsProvided . item) decls
-}

moduleInternalSymbols' :: ( ProjectModuleClass m
                          , ProjectExternModuleClass m
                          , ModuleDeclarationClass m
                          , ModuleImportClass m
                          , ModuleExportClass m
                          , ExternModuleExportClass m
                          )
                => ProjectInfo 
                -> ModuleInfo
                -> SolutionResult m u [Symbol]
moduleInternalSymbols' pji mi = do
    localSyms <- moduleDeclaredSymbols pji mi
    importSyms <- moduleImportedSymbols' pji mi
    return $ importSyms ++ localSyms

moduleDeclaredSymbols :: (ProjectModuleClass m, ModuleDeclarationClass m)
                      => ProjectInfo
                      -> ModuleInfo
                      -> SolutionResult m u [Symbol]
moduleDeclaredSymbols pji mi = do
    dis <- getDeclarations pji mi
    ds <- mapM (liftM item . getDeclaration pji mi) dis
    return $ concatMap Declaration.symbolsProvided ds
    
{-
-- | Given a sub-symbol, (such as a data constructor or a class method), find
--  the parent symbol and its siblings
--  If successful, the list will contain the parent symbol as its head, and the
--      siblings as the tail. The symbol provided will not be an item in the list
--  If the symbol is imported, it will be tagged as such
moduleSymbolTree :: (ProjectModuleClass m, ProjectExternModuleClass m)
           => ProjectInfo
           -> Module 
           -> Symbol 
           -> SolutionResult m u [ModuleChild Symbol]
moduleSymbolTree pji m sym = do
    let declarations = items $ Map.elems $ moduleDeclarations m
        declSearchResult = Module.search (`Declaration.otherSymbols` sym) declarations
    case declSearchResult of
        Just (_,syms) -> return $ map (Module.qualify m) syms
        Nothing -> do
            let imports = items $ Map.elems $ moduleImports m
                othersFromImport x = importOtherSymbols pji x sym
            importSearchResult <- Module.searchM othersFromImport imports
            case importSearchResult of
                Just (i,_) -> do
                    otherSyms <- importSymbolTree pji i sym
                    return $ map (Module.qualify m) otherSyms
                Nothing -> throwE $ SymbolNotFound (Module.info m) sym "moduleSymbolTree"
-}

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
           -> SolutionResult m u [ModuleChild Symbol]
moduleSymbolTree' pji mi sym = do
    declarations <- liftM items $ getDeclarations pji mi >>= mapM (getDeclaration pji mi)
    let declSearchResult = Module.search (`Declaration.otherSymbols` sym) declarations
    case declSearchResult of
        Just (_,syms) -> return $ map (ModuleChild mi) syms
        Nothing -> do
            imports <- liftM items $ getImports pji mi >>= mapM (getImport pji mi) 
            let othersFromImport x = importOtherSymbols' pji x sym
            importSearchResult <- Module.searchM othersFromImport imports
            case importSearchResult of
                Just (i,_) -> do
                    otherSyms <- importSymbolTree' pji i sym
                    return $ map (ModuleChild mi) otherSyms
                Nothing -> throwE $ SymbolNotFound mi sym "moduleSymbolTree"
{-
-- | Get either an internal or external module
getAnyModule :: (ProjectModuleClass m, ProjectExternModuleClass m)
             => ProjectInfo 
             -> ModuleInfo 
             -> SolutionResult m u EitherModule
getAnyModule pji mi = catchE tryLocal $ const tryExtern
  where
    tryLocal = liftM Left $ getModule pji mi
    tryExtern = liftM Right $ getExternModule pji mi
-}
    

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
                   -> SolutionResult m u  [Symbol]
getExternalSymbols pji mi = liftM (map getChild) $ eitherModuleExportedSymbols' pji mi
{-pji mi = do
    m <- getAnyModule pji mi
    case m of
        Left lm -> liftM (map getChild) $ moduleExportedSymbols pji lm
        Right em -> return $ map getChild $ ExternModule.exportedSymbols em-}

{-
getExternalSymbols' :: (ProjectModuleClass m, ProjectExternModuleClass m)
                   => ProjectInfo 
                   -> ModuleInfo 
                   -> SolutionResult m u  [Symbol]
getExternalSymbols' pji mi = do
    hasEM <- hasExternModule pji mi
    if hasEM
        then liftM (map getChild) $ getExternExports pji mi >>= mapM getexportedSymbols em
        else liftM (map getChild) $ moduleExportedSymbols' pji mi
-}

-- | Get the symbols availible at the top level of a module
getInternalSymbols :: ( ProjectModuleClass m
                     , ProjectExternModuleClass m
                     , ModuleImportClass m
                     , ModuleDeclarationClass m
                     , ModuleExportClass m
                     , ExternModuleExportClass m
                     )
                   => ProjectInfo 
                   -> ModuleInfo 
                   -> SolutionResult m u  [Symbol]
getInternalSymbols pji mi = do
    localSyms <- moduleDeclaredSymbols pji mi
    importedSyms <- moduleImportedSymbols' pji mi
    return $ localSyms ++ importedSyms


{-
-- | Get the symbols exported by either a local or external module, annotated
-- with that module's identifying information
eitherModuleExportedSymbols :: (ProjectModuleClass m, ProjectExternModuleClass m)
                => ProjectInfo
                -> EitherModule 
                -> SolutionResult m u [ModuleChild Symbol]
eitherModuleExportedSymbols pji m = case m of
    Left lm -> moduleExportedSymbols pji lm
    Right em -> return $ ExternModule.exportedSymbols em
-}

hasModule :: (ProjectModuleClass m) 
          => ProjectInfo 
          -> ModuleInfo 
          -> SolutionResult m u Bool
hasModule pji mi = liftM (mi `elem`) $ getModules pji

hasExternModule :: (ProjectExternModuleClass m)
                => ProjectInfo
                -> ModuleInfo
                -> SolutionResult m u Bool
hasExternModule pji mi = liftM (mi `elem`) $ getExternModules pji

eitherModuleExportedSymbols' :: ( ProjectModuleClass m
                                , ProjectExternModuleClass m
                                , ExternModuleExportClass m
                                , ModuleExportClass m
                                , ModuleDeclarationClass m
                                , ModuleImportClass m
                                )
                => ProjectInfo
                -> ModuleInfo
                -> SolutionResult m u [ModuleChild Symbol]
eitherModuleExportedSymbols' pji mi = do
    hasEM <- hasExternModule pji mi
    if hasEM
        then externModuleExportedSymbols' pji mi
        else moduleExportedSymbols' pji mi

externModuleExportedSymbols' :: ( ProjectExternModuleClass m
                                , ExternModuleExportClass m
                                )
                            => ProjectInfo
                            -> ModuleInfo
                            -> SolutionResult m u [ModuleChild Symbol]
externModuleExportedSymbols' pji mi = do
    es <- getExternExports pji mi >>= mapM (getExternExport pji mi)
    return $ map (ModuleChild mi) $ concatMap ExternModule.exportSymbols es
{-
-- | Given a symbol such as a class method of data constructor, find the rest
-- of the related symbols exported by a local or external module.
-- The result list's head is the data type/class, and the tail as the other
-- symbols
-- This does not return the symbol provided
eitherModuleSymbolTree :: (ProjectModuleClass m, ProjectExternModuleClass m)
           => ProjectInfo
           -> EitherModule 
           -> Symbol 
           -> SolutionResult m u [ModuleChild Symbol]
eitherModuleSymbolTree pji m s = case m of
    Left lm -> moduleSymbolTree pji lm s
    Right em -> externModuleSymbolTree em s
-}

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
           -> SolutionResult m u [ModuleChild Symbol]
eitherModuleSymbolTree' pji mi s = do
    hasEM <- hasExternModule pji mi
    if hasEM
        then externModuleSymbolTree' pji mi s
        else moduleSymbolTree' pji mi s


{-
-- | Given a symbol such as a class method of data constructor, find the rest
-- of the related symbols exported by an external module.
-- The result list's head is the data type/class, and the tail as the other
-- symbols
-- This does not return the symbol provided
externModuleSymbolTree :: Monad m => ExternModule -> Symbol -> SolutionResult m u [ModuleChild Symbol]
externModuleSymbolTree m s = case getFirst $ mconcat $ map look es of
    Just ss -> return $ map (ModuleChild i) ss
    Nothing -> throwE $ SymbolNotExported i s "Extern.symbolTree"
  where
    es = Map.elems $ externModuleExports m
    i = ExternModule.info m
    look (SingleExternExport s') | s == s' = First $ Just []
    look (MultiExternExport s' ss) | s' `elem` ss = First $ Just $ delete s' ss
    look _ = First Nothing
-}
externModuleSymbolTree' :: ( ExternModuleExportClass m 
                           )
                        => ProjectInfo
                        -> ModuleInfo 
                        -> Symbol 
                        -> SolutionResult m u [ModuleChild Symbol]
externModuleSymbolTree' pji mi s = do
    es <- getExternExports pji mi >>= mapM (getExternExport pji mi)
    case getFirst $ mconcat $ map look es of
        Just ss -> return $ map (ModuleChild mi) ss
        Nothing -> throwE $ SymbolNotExported mi s "Extern.symbolTree"
  where
    look (SingleExternExport s') | s == s' = First $ Just []
    look (MultiExternExport s' ss) | s' `elem` ss = First $ Just $ delete s' ss
    look _ = First Nothing
