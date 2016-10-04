{-# LANGUAGE FlexibleContexts #-}
module SolutionTree where

import Data.Monoid

import Data.Tree
import Data.List

import Control.Monad
import Control.Monad.Trans

import Ide3.Types
import Ide3.ModuleTree hiding (makeModuleTree)
import Ide3.NewMonad
import Ide3.Utils

import GuiClass

import ViewerMonad

import DeclarationPath

data TreeSearchResult
    = ProjectResult ProjectInfo
    | ModuleResult ProjectInfo ModuleInfo Bool
    | DeclResult ProjectInfo ModuleInfo DeclarationInfo
    | ImportsResult ProjectInfo ModuleInfo
    | ExportsResult ProjectInfo ModuleInfo
    | PramgasResult ProjectInfo ModuleInfo
    | ImportResult ProjectInfo ModuleInfo ImportId
    | ExportResult ProjectInfo ModuleInfo ExportId
    | PragmaResult ProjectInfo ModuleInfo Pragma
    | NoSearchResult

{-
newtype PSW a = PSW { unPSW :: SolutionStateT IO a }
    deriving (Functor,Applicative,Monad,SolutionStateM,MonadIO)

instance SolutionShellM PSW where
    load = error "STOP"
    new = error "STOP"
    finalize = error "STOP"
-}




makePragmasNode :: [Pragma] -> Tree SolutionTreeElem
makePragmasNode ps = Node PragmasElem $ map (flip Node [] . PragmaElem) ps

makeImportsNode :: [(ImportId,WithBody Import)] -> Tree SolutionTreeElem
makeImportsNode is = Node ImportsElem $ map (flip Node [] . uncurry ImportElem) is

makeExportsNode :: Maybe [(ExportId,WithBody Export)] -> Tree SolutionTreeElem
makeExportsNode (Just es) = Node ExportsElem $ map (flip Node [] . uncurry ExportElem) es
makeExportsNode Nothing = Node ExportsElem []

makeModuleTree :: ModuleTree -> Tree SolutionTreeElem
makeModuleTree (OrgNode mi ts)
    = Node (ModuleElem mi False) $ map makeModuleTree ts
makeModuleTree (ModuleNode mi ts ps ds is es) 
    = Node (ModuleElem mi True) 
    $ makePragmasNode ps
    : makeImportsNode is 
    : makeExportsNode es 
    : map makeModuleTree ts 
    ++ map (flip Node [] . DeclElem) ds 
    
makeProjectTree :: ProjectInfo -> [ModuleTree] -> Tree SolutionTreeElem
makeProjectTree pi branches = Node (ProjectElem pi) $ map makeModuleTree branches

makeSolutionTree :: [(ProjectInfo,[ModuleTree])] -> Forest SolutionTreeElem
makeSolutionTree = map (uncurry makeProjectTree)

searchTree
    :: ( SolutionViewClass m --t (SolutionResult u m)
       ) 
    => SolutionPath
    -> m {-t (SolutionResult u m)-} [TreePath]
searchTree path = do
    --tree <- treeStoreGetTree store [0]
    trees <- getForestAtSolutionPath SolutionPath
    return $ evalMultiState (searchTree' path) $ Node undefined trees

{-
searchTree' :: DeclarationPath -> Tree SolutionTreeElem -> [TreePath]
searchTree' path tree = map fst $ case path of
    DeclarationPath{} -> declMatches
    ExportPath{} -> exportMatches
    ImportPath{} -> importMatches
    PragmaPath{} -> pragmaMatches
    ModulePath{} -> moduleMatches
    ProjectPath{} -> map (\(i,x) -> ([i],x)) projectMatches
  where
    (pi,mi,di,ii,ei,p) = case path of
        DeclarationPath pi mi di -> (pi,mi,di,undefined,undefined,undefined)
        ImportPath pi mi ii -> (pi,mi,undefined,ii,undefined,undefined)
        ExportPath pi mi ie -> (pi,mi,undefined,undefined,ei,undefined)
        PragmaPath pi mi pri -> (pi,mi,undefined,undefined,undefined,p)
        ModulePath pi mi -> (pi,mi,undefined,undefined,undefined,undefined)
        ProjectPath pi -> (pi,undefined,undefined,undefined,undefined,undefined)
    projectMatches = searchTreeForProject pi tree
    moduleMatches = flip concatMap projectMatches 
        $ \(projectIndex, projectTree) -> flip map (searchTreeForModule mi $ projectTree)
            $ \(moduleIndices, moduleTree) -> (projectIndex : moduleIndices, moduleTree)
    declMatches = flip concatMap moduleMatches 
        $ \(moduleIndices, moduleTree) -> flip map (searchTreeForDeclaration di $ moduleTree)
            $ \(declarationIndex, declarationTree) -> (moduleIndices ++ [declarationIndex], declarationTree)
    importMatches = flip concatMap moduleMatches
        $ \(moduleIndices, moduleTree) -> flip map (searchTreeForImport ii $ moduleTree)
            $ \(importIndex, importTree) -> (moduleIndices ++ [importIndex], importTree)
    exportMatches = flip concatMap moduleMatches
        $ \(moduleIndices, moduleTree) -> flip map (searchTreeForExport ei $ moduleTree)
            $ \(exportIndex, exportTree) -> (moduleIndices ++ [exportIndex], exportTree)    
    pragmaMatches = flip concatMap moduleMatches
        $ \(moduleIndices, moduleTree) -> flip map (searchTreeForPragma p $ moduleTree)
            $ \(pragmaIndex, pragmaTree) -> (moduleIndices ++ [pragmaIndex], pragmaTree)
-}

searchTree' :: SolutionPath -> MultiState (Tree SolutionTreeElem) TreePath
searchTree' (DeclarationPath pji mi di)
    =   searchTreeForProject pji 
    >-> searchTreeForModule mi 
    >-> searchTreeForDeclaration di
searchTree' (ExportPath pji mi ei)
    =   searchTreeForProject pji 
    >-> searchTreeForModule mi 
    >-> searchTreeForExports 
    >-> searchTreeForExport ei
searchTree' (ImportPath pji mi ii)
    =   searchTreeForProject pji
    >-> searchTreeForModule mi
    >-> searchTreeForImports
    >-> searchTreeForImport ii
searchTree' (PragmaPath pji mi p)
    =   searchTreeForProject pji
    >-> searchTreeForModule mi
    >-> searchTreeForPragmas
    >-> searchTreeForPragma p
searchTree' (ExportsPath pji mi)
    =   searchTreeForProject pji
    >-> searchTreeForModule mi
    >-> searchTreeForExports
searchTree' (ImportsPath pji mi)
    =   searchTreeForProject pji
    >-> searchTreeForModule mi
    >-> searchTreeForImports
searchTree' (PragmasPath pji mi)
    =   searchTreeForProject pji
    >-> searchTreeForModule mi
    >-> searchTreeForPragmas
searchTree' (ModulePath pji mi)
    =   searchTreeForProject pji
    >-> searchTreeForModule mi
searchTree' (ProjectPath pji)
    =   searchTreeForProject pji

newtype MultiState s a = MultiState { runMultiState :: s -> [(a,s)] }

instance Functor (MultiState s) where
    fmap f g = MultiState $ \s -> map (\(x,s') -> (f x,s')) $ runMultiState g s

instance Monad (MultiState s) where
    return x = MultiState $ \s -> [(x,s)]
    f >>= g = MultiState $ \s ->
        let results = runMultiState f s
        in concatMap (\(x,s') -> runMultiState (g x) s') results

instance Applicative (MultiState s) where
    pure = return
    (<*>) = ap

getMulti :: MultiState s s
getMulti = MultiState $ \s -> [(s,s)]

putMulti :: [s] -> MultiState s ()
putMulti ss = MultiState $ \_ -> map (\s -> ((),s)) ss

modifyMulti :: [s -> s] -> MultiState s ()
modifyMulti fs = MultiState $ \s -> map (\f -> ((),f s)) fs

evalMultiState :: MultiState s a -> s -> [a]
evalMultiState f s = map fst $ runMultiState f s

{-
searchTreeForModulePart :: ModuleInfo -> Tree SolutionTreeElem -> [(Int, Tree SolutionTreeElem)]
searchTreeForModulePart (ModuleInfo (Symbol s)) = searchTree''' moduleNameIsPrefixOf
  where
    moduleNameIsPrefixOf (ModuleElem (ModuleInfo (Symbol s')) _) = s' `isPrefixOf` s && case drop (length s') s of
        [] -> True
        ('.':_) -> True
        _ -> False
    moduleNameIsPrefixOf _ = False
-}

searchTreeForModulePart :: ModuleInfo -> MultiState (Tree SolutionTreeElem) TreePath
searchTreeForModulePart (ModuleInfo (Symbol s)) = searchTree''' moduleNameIsPrefixOf
  where
    moduleNameIsPrefixOf mElem
        | (ModuleElem mi _) <- mElem
        , (ModuleInfo sym) <- mi
        , (Symbol s') <- sym
        = s' `isPrefixOf` s && case drop (length s') s of
            [] -> True
            ('.':_) -> True
            _ -> False
    moduleNameIsPrefixOf _ = False
searchTreeForModulePart _ = MultiState $ \s -> []

{-
searchTreeForModule :: ModuleInfo -> Tree SolutionTreeElem -> [(TreePath, Tree SolutionTreeElem)]
searchTreeForModule mi tree = case searchTree''' matchesModule tree of
    [] -> let partMatches = searchTreeForModulePart mi tree
          in flip concatMap partMatches $ 
                \(i, nextTree) -> map (\(is,moduleTree) -> (i:is,moduleTree)) 
                             $ searchTreeForModule mi nextTree
    xs -> map (\(i, tree') -> ([i],tree')) xs
  where
    matchesModule (ModuleElem mi' _) = mi == mi'
    matchesModule _ = False
-}

searchTreeForModule :: ModuleInfo -> MultiState (Tree SolutionTreeElem) TreePath
searchTreeForModule mi = MultiState $ \s -> 
    case runMultiState (searchTree''' matchesModule) s of
        [] -> runMultiState (searchTreeForModulePart mi >-> searchTreeForModule mi) s
        xs -> xs
  where
    matchesModule (ModuleElem mi' _) = mi == mi'
    matchesModule _ = False

{-
searchTreeForDeclaration :: DeclarationInfo -> Tree SolutionTreeElem -> [(Int, Tree SolutionTreeElem)]
searchTreeForDeclaration di = searchTree''' matchesDeclaration
  where
    matchesDeclaration = ((DeclElem di) ==)
-}

searchTreeForDeclaration :: DeclarationInfo -> MultiState (Tree SolutionTreeElem) TreePath
searchTreeForDeclaration di = searchTree''' ((DeclElem di) ==)

{-
searchTreeForImport :: ImportId -> Tree SolutionTreeElem -> [(Int, Tree SolutionTreeElem)]
searchTreeForImport ii = searchTree''' matchesImport
  where
    matchesImport (ImportElem ii' _) = ii == ii'
    matchesImport _ = False
-}

searchTreeForImport :: ImportId -> MultiState (Tree SolutionTreeElem) TreePath
searchTreeForImport ii = searchTree''' matchesImport
  where
    matchesImport (ImportElem ii' _) = ii == ii'
    matchesImport _ = False

{-
searchTreeForExport :: ExportId -> Tree SolutionTreeElem -> [(Int, Tree SolutionTreeElem)]
searchTreeForExport ei = searchTree''' matchesExport
  where
    matchesExport (ExportElem ei' _) = ei == ei'
    matchexExport _ = False
-}

searchTreeForExport :: ExportId -> MultiState (Tree SolutionTreeElem) TreePath
searchTreeForExport ei = searchTree''' matchesExport
  where
    matchesExport (ExportElem ei' _) = ei == ei'
    matchesExport _ = False

{-
searchTreeForExports :: Tree SolutionTreeElem -> [(Int, Tree SolutionTreeElem)]
searchTreeForExports = searchTree''' matchesExports
  where
    matchesExports = (ExportsElem ==)
-}

searchTreeForExports :: MultiState (Tree SolutionTreeElem) TreePath
searchTreeForExports = searchTree''' (ExportsElem ==)

{-
searchTreeForImports :: Tree SolutionTreeElem -> [(Int, Tree SolutionTreeElem)]
searchTreeForImports = searchTree''' matchesImports
  where
    matchesImports = (ImportsElem ==)
-}

searchTreeForImports :: MultiState (Tree SolutionTreeElem) TreePath
searchTreeForImports = searchTree''' (ImportsElem ==)

{-
searchTreeForPragmas :: Tree SolutionTreeElem -> [(Int, Tree SolutionTreeElem)]
searchTreeForPragmas = searchTree''' matchesPragmas
  where
    matchesPragmas = (PragmasElem ==)
-}

searchTreeForPragmas :: MultiState (Tree SolutionTreeElem) TreePath
searchTreeForPragmas = searchTree''' (PragmasElem ==)

{-
searchTreeForPragma :: Pragma -> Tree SolutionTreeElem -> [(Int, Tree SolutionTreeElem)]
searchTreeForPragma p = searchTree''' matchesPragma
  where
    matchesPragma = ((PragmaElem p) ==)
-}

searchTreeForPragma :: Pragma -> MultiState (Tree SolutionTreeElem) TreePath
searchTreeForPragma p = searchTree''' ((PragmaElem p) ==)

{-
searchTreeForProject :: ProjectInfo -> Tree SolutionTreeElem -> [(Int, Tree SolutionTreeElem)]
searchTreeForProject pi = searchTree''' matchesProject
  where
    matchesProject = ((ProjectElem pi) ==)
-}

searchTreeForProject :: ProjectInfo -> MultiState (Tree SolutionTreeElem) TreePath
searchTreeForProject pji = searchTree''' ((ProjectElem pji) ==)


{-
searchTree''' :: (SolutionTreeElem -> Bool) -> Tree SolutionTreeElem -> [(Int, Tree SolutionTreeElem)]
searchTree''' f tree = filter (f . rootLabel . snd) $ zip [0..] $ subForest tree
-}

searchStep :: Monoid m => MultiState s m -> MultiState s m -> MultiState s m
searchStep f g = do
    x <- f
    y <- g 
    return $ x <> y

(>->) :: Monoid m => MultiState s m -> MultiState s m -> MultiState s m
(>->) = searchStep

searchTree''' :: (SolutionTreeElem -> Bool) -> MultiState (Tree SolutionTreeElem) TreePath
searchTree''' f = liftM (:[]) $ MultiState $ \tree -> 
    filter (f . rootLabel . snd) $ zip [0..] $ subForest tree

populateTree :: ( MonadTrans t
                , MonadSplice t
                , Monad (t (SolutionResult u m))
                , SolutionViewClass (t m)
                , ViewerMonad m
                , SolutionClass m
                , ProjectModuleClass m
                , ModuleExportClass m
                , ModuleImportClass m
                , ModuleDeclarationClass m
                , ModulePragmaClass m
                ) 
             => t (SolutionResult u m) ()
populateTree = do
    projects <- lift getProjects
    trees <- lift $ forM projects $ \pi -> do
        tree <- makeTree pi
        return (pi, tree)
    --let forest = makeSolutionTree trees
    --liftIO $ do
        --treeStoreClear treeStore
        --treeStoreInsertForest treeStore [] 0 forest
    splice $ setSolutionTree $ makeSolutionTree trees

findAtPath :: ( SolutionViewClass m
              ) => TreePath -> m TreeSearchResult
findAtPath path = do
    let parentPath = case path of
            [] -> []
            _ -> init path
        grandparentPath = case path of
            [] -> []
            [x] -> [x]
            _ -> init $ init path
        ancestorPath = case path of
            [] -> []
            (x:_) -> [x]
    --node <- treeStoreGetValue treeStore path
    node <- getElemAtSolutionTreePath path
    --parentNode <- treeStoreGetValue treeStore parentPath
    parentNode <- getElemAtSolutionTreePath parentPath
    --grandparentNode <- treeStoreGetValue treeStore grandparentPath
    grandparentNode <- getElemAtSolutionTreePath grandparentPath
    --ancestorNode <- treeStoreGetValue treeStore ancestorPath
    ancestorNode <- getElemAtSolutionTreePath ancestorPath
    case (node,parentNode,grandparentNode,ancestorNode) of
        (ProjectElem pi,_,_,_) -> return $ ProjectResult pi
        (ModuleElem mi b,_,_,ProjectElem pi) -> return $ ModuleResult pi mi b
        (DeclElem di,ModuleElem mi b,_,ProjectElem pi) -> return $ DeclResult pi mi di
        (ImportsElem,ModuleElem mi b,_,ProjectElem pi) -> return $ ImportsResult pi mi
        (ExportsElem,ModuleElem mi b,_,ProjectElem pi) -> return $ ExportsResult pi mi
        (ImportElem ii _,_,ModuleElem mi b,ProjectElem pi) -> return $ ImportResult pi mi ii
        (ExportElem ei _,_,ModuleElem mi b,ProjectElem pi) -> return $ ExportResult pi mi ei
        _ -> return NoSearchResult

{-
getModuleAndDecl :: TreePath -> TreeStore SolutionTreeElem -> IO (Maybe (ModuleInfo,DeclarationInfo))
getModuleAndDecl path treeStore = do
    node <- treeStoreGetValue treeStore path 
    case node of
        DeclElem di -> do
            let parentPath = init path
            parentNode <- treeStoreGetValue treeStore parentPath
            case parentNode of
                ModuleElem mi -> return $ Just (mi,di)
                _ -> return Nothing 
        _ -> return Nothing

getModuleAndImport :: TreePath -> TreeStore SolutionTreeElem -> IO (Maybe (ModuleInfo,ImportId))
getModuleAndImport path treeStore = do
    node <- treeStoreGetValue treeStore path 
    case node of
        ImportElem ii _ -> do
            let parentPath = init path
            parentNode <- treeStoreGetValue treeStore parentPath
            case parentNode of
                ModuleElem mi -> return $ Just (mi,ii)
                _ -> return Nothing 
        _ -> return Nothing

getModuleAndExport :: TreePath -> TreeStore SolutionTreeElem -> IO (Maybe (ModuleInfo,ExportId))
getModuleAndExport path treeStore = do
    node <- treeStoreGetValue treeStore path 
    case node of
        ExportElem ei _ -> do
            let parentPath = init path
            parentNode <- treeStoreGetValue treeStore parentPath
            case parentNode of
                ModuleElem mi -> return $ Just (mi,ei)
                _ -> return Nothing 
        _ -> return Nothing
-}


getElemAtSolutionPath :: SolutionViewClass m => SolutionPath -> m SolutionTreeElem
getElemAtSolutionPath spath = do
    [tpath] <- searchTree spath
    getElemAtSolutionTreePath tpath
getTreeAtSolutionPath :: SolutionViewClass m => SolutionPath -> m (Tree SolutionTreeElem)
getTreeAtSolutionPath spath = do
    [tpath] <- searchTree spath
    getTreeAtSolutionTreePath tpath
getForestAtSolutionPath :: SolutionViewClass m => SolutionPath -> m (Forest SolutionTreeElem)
getForestAtSolutionPath spath = do
    [tpath] <- searchTree spath
    getForestAtSolutionTreePath tpath
lookupAtSolutionPath :: SolutionViewClass m => SolutionPath -> m (Maybe (Tree SolutionTreeElem))
lookupAtSolutionPath spath = do
    result <- searchTree spath
    case result of
        [] -> return Nothing
        [tpath] -> lookupAtSolutionTreePath tpath
updateSolutionTreeNode :: SolutionViewClass m => SolutionPath -> (SolutionTreeElem -> SolutionTreeElem) -> m ()
updateSolutionTreeNode spath f = do
    [tpath] <- searchTree spath
    updateSolutionTreePathNode tpath f
insertSolutionTreeNode :: SolutionViewClass m => SolutionPath -> SolutionTreeElem -> m ()
insertSolutionTreeNode spath elem = do
    [tpath] <- searchTree spath
    insertSolutionTreePathNode tpath Nothing elem
insertSolutionTreeTree :: SolutionViewClass m => SolutionPath -> Tree SolutionTreeElem -> m ()
insertSolutionTreeTree spath tree = do
    [tpath] <- searchTree spath
    insertSolutionTreePathTree tpath Nothing tree
removeSolutionTreeNode :: SolutionViewClass m => SolutionPath -> m ()
removeSolutionTreeNode spath = do
    [tpath] <- searchTree spath
    removeSolutionTreePathNode tpath
