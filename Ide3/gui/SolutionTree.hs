{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
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
    | UnparsableModuleResult ProjectInfo ModuleInfo
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
makeModuleTree (UnparsableModuleNode mi ts _)
    = Node (UnparsableModuleElem mi) $ map makeModuleTree ts
    
makeProjectTree :: ProjectInfo -> [ModuleTree] -> Tree SolutionTreeElem
makeProjectTree pi branches = Node (ProjectElem pi) $ map makeModuleTree branches

makeSolutionTree :: [(ProjectInfo,[ModuleTree])] -> Forest SolutionTreeElem
makeSolutionTree = map (uncurry makeProjectTree)

getModuleParent :: SolutionViewClass m => ProjectInfo -> ModuleInfo -> m (Maybe ModuleInfo)
getModuleParent pji mi = do
    paths <- getModuleParentPath pji mi
    case paths of
        [path] -> do
            item <- getElemAtSolutionTreePath path
            case item of
                ModuleElem mi' _ -> return $ Just mi'
                UnparsableModuleElem mi' -> return $ Just mi'
                _ -> return Nothing            
        _ -> return Nothing

getModuleParentPath :: SolutionViewClass m => ProjectInfo -> ModuleInfo -> m [TreePath]
getModuleParentPath pji mi = do
    trees <- getForestAtSolutionPath SolutionPath
    return $ flip evalMultiState (Node undefined trees) $ do
        searchTree' $ ProjectPath pji
        searchTreeForModuleWithChild mi

searchTree
    :: ( SolutionViewClass m --t (SolutionResult u m)
       ) 
    => SolutionPath
    -> m {-t (SolutionResult u m)-} [TreePath]
searchTree SolutionPath = return [[]]
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
searchTree' (UnparsableModulePath pji mi)
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
        | (UnparsableModuleElem mi) <- mElem
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
    matchesModule (UnparsableModuleElem mi') = mi == mi'
    matchesModule _ = False

searchTreeForModuleWithChild :: ModuleInfo -> MultiState (Tree SolutionTreeElem) TreePath
searchTreeForModuleWithChild mi = MultiState $ \s ->
    case runMultiState (searchTree''' matchesModule) s of
        [] -> runMultiState (searchTreeForModulePart mi >-> searchTreeForModuleWithChild mi) s
        xs -> []
  where
    matchesModule (ModuleElem mi' _) = mi == mi'
    matchesModule (UnparsableModuleElem mi') = mi == mi'
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
    node <- case path of
        (_:_) -> liftM Just $ getElemAtSolutionTreePath path
        [] -> return Nothing
    parentNode <- case path of
        (_:_:_) -> liftM Just $ getElemAtSolutionTreePath $ init path
        _ -> return Nothing
    grandparentNode <- case path of
        (_:_:_:_) -> liftM Just $ getElemAtSolutionTreePath $ init $ init path
        _ -> return Nothing
    ancestorNode <- case path of
        (ancestor:_) -> liftM Just $ getElemAtSolutionTreePath [ancestor]
        _ -> return Nothing
    case (node,parentNode,grandparentNode,ancestorNode) of
        (Just (ProjectElem pi),_,_,_) -> return $ ProjectResult pi
        (Just (ModuleElem mi b),_,_,Just (ProjectElem pi)) -> return $ ModuleResult pi mi b
        (Just (UnparsableModuleElem mi),_,_,Just (ProjectElem pi)) -> return $ UnparsableModuleResult pi mi
        (Just (DeclElem di),Just (ModuleElem mi b),_,Just (ProjectElem pi)) -> return $ DeclResult pi mi di
        (Just ImportsElem,Just (ModuleElem mi b),_,Just (ProjectElem pi)) -> return $ ImportsResult pi mi
        (Just ExportsElem, Just (ModuleElem mi b),_,Just (ProjectElem pi)) -> return $ ExportsResult pi mi
        (Just (ImportElem ii _),_, Just (ModuleElem mi b) ,Just (ProjectElem pi)) -> return $ ImportResult pi mi ii
        (Just (ExportElem ei _),_, Just (ModuleElem mi b), Just (ProjectElem pi)) -> return $ ExportResult pi mi ei
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
updateSolutionTreeTree :: SolutionViewClass m => SolutionPath -> (Tree SolutionTreeElem -> Tree SolutionTreeElem) -> m ()
updateSolutionTreeTree spath f = do
    [tpath] <- searchTree spath
    tree <- getTreeAtSolutionTreePath tpath
    let tree' = f tree
    removeSolutionTreePathNode tpath
    insertSolutionTreePathTree (init tpath) (Just $ last tpath) tree'

hasSubModules' :: SolutionViewClass m => SolutionPath -> m Bool
hasSubModules' path = flip any <$> getTreeAtSolutionPath path <*> ( pure $ \case
        (ModuleElem _ _) -> True
        (UnparsableModuleElem _) -> True
        _ -> False)

hasSubModules :: SolutionViewClass m => SolutionPath -> m Bool
hasSubModules path@(ModulePath pji mi) = hasSubModules' path
hasSubModules path@(UnparsableModulePath pji mi) = hasSubModules' path
hasSubModules _ = return False


-- | Remove the module from the tree, but keeping it
-- and any parents that have other sub-modules, then
-- re-add it based on its new module name
moveModuleInTree :: SolutionViewClass m 
                 => ProjectInfo 
                 -> ModuleInfo 
                 -> ModuleInfo 
                 -> Tree SolutionTreeElem
                 -> m ()
moveModuleInTree pji mi mi' sTree = do
    removeModuleFromTree pji mi
    addModuleToTree pji mi' sTree


-- | Remove a module from the tree, preserving its sub-modules, and leaving an
-- empty module node if any exist, but removing it and any empty parent module
-- nodes
removeModuleFromTree :: SolutionViewClass m => ProjectInfo -> ModuleInfo -> m ()
removeModuleFromTree = go True
  where
    -- Walk from the module to delete towards the root
    go isFirst pji mi = do
        pred <- hasSubModules (ModulePath pji mi)
        if pred
            -- If the current node has sub-modules
            then updateSolutionTreeTree (ModulePath pji mi) $ if isFirst
                    -- If the current node is the module to delete, replace it
                    -- with a structural node with only the sub-trees that
                    -- contain modules, then halt the walk
                    then \case
                        Node (ModuleElem _ _) ts -> Node (ModuleElem mi True) $ flip filter ts $ \case
                            Node ModuleElem{} _ -> True
                            Node UnparsableModuleElem{} _ -> True
                            _ -> False
                        Node (UnparsableModuleElem _) ts -> Node (ModuleElem mi True) ts
                    -- If the current node is a parent of the deleted node, leave
                    -- it as is, then halt the walk
                    else id
            -- If the current node has no sub-modules
            else do
                item <- getElemAtSolutionPath $ ModulePath pji mi
                case item of
                    -- If the current node is a sturctural node, remove it, then
                    -- walk up one level
                    ModuleElem _ True -> do
                        removeSolutionTreeNode $ ModulePath pji mi
                        mi' <- getModuleParent pji mi
                        case mi' of
                            Just mi' -> go False pji mi'
                            Nothing -> return ()
                    -- If the current node is not structural, halt the walk
                    _ -> return ()

-- | Add a module to the tree, along with any needed structural nodes, merging
-- with any existing node for that module
addModuleToTree :: SolutionViewClass m => ProjectInfo -> ModuleInfo -> Tree SolutionTreeElem -> m ()
addModuleToTree pji mi = loop Nothing
  where
    --   We now have a tree rooted at the ancestor of the new module, and each
    -- of the trees have a single branch, pointing to the next ancestor, until
    -- it reaches the node for the new module.
    
    --   Because there may be orgnaizational nodes that need to be added, we
    -- traverse down the tree, checking the stored tree if there is already a
    -- node with that path. If there is, we go to the next descendent.
    --   Once we reach a node that is not present, that is the top of the tree
    -- that contains the new module, so we insert that tree wholesale.
    loop parent tree@(Node item trees') = do
        result <- case item of
            ModuleElem mi' _ -> lookupAtSolutionPath $ ModulePath pji mi'
            UnparsableModuleElem mi' -> lookupAtSolutionPath $ UnparsableModulePath pji mi'
        let mi' = case item of
                ModuleElem mi' _ -> mi'
                UnparsableModuleElem mi' -> mi'
            oldPath = case item of
                ModuleElem _ _ -> ModulePath pji mi
                UnparsableModuleElem _ -> UnparsableModulePath pji mi
        case (result, trees') of
            -- If the path isn't there, this is the root of the new part
            (Nothing,_) -> insertSolutionTreeTree parentPath tree
            -- If the path is not known, the infos do not match, and there
            -- is one possible sub-module to traverse, this module is known,
            -- traverse to the next level
            (_, [tree']) | mi /= mi' -> loop (Just mi') tree'
            -- If the path is known, but the infos match, then the module is
            -- being updated, change it to a non-structural node with both
            -- the old and new sub-trees
            (_, _) -> updateSolutionTreeTree oldPath $ 
                \(Node _ trees) -> Node (ModuleElem mi False) $ trees' ++ trees
      where
        parentPath = maybe (ProjectPath pji) (ModulePath pji) parent
