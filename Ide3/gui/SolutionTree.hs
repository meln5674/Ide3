{-# LANGUAGE FlexibleContexts #-}
module SolutionTree where

import Data.Tree
import Data.List

import Control.Monad
import Control.Monad.Trans

import Ide3.Types
import Ide3.ModuleTree
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

makeSolutionTree :: [(ProjectInfo,[ModuleTree])] -> [Tree SolutionTreeElem]
makeSolutionTree = map (uncurry makeProjectTree)

searchTree
    :: ( SolutionViewClass m --t (SolutionResult u m)
       ) 
    => DeclarationPath
    -> m {-t (SolutionResult u m)-} [TreePath]
searchTree path = do
    --tree <- treeStoreGetTree store [0]
    trees <- getForestAtSolutionPath []
    return $ searchTree' path $ Node undefined trees

searchTree' :: DeclarationPath -> Tree SolutionTreeElem -> [TreePath]
searchTree' path tree = map fst $ case path of
    DeclarationPath{} -> declMatches
    ModulePath{} -> moduleMatches
    ProjectPath{} -> map (\(i,x) -> ([i],x)) projectMatches
  where
    (pi,mi,di) = case path of
        DeclarationPath pi mi di -> (pi,mi,di)
        ModulePath pi mi -> (pi,mi,undefined)
        ProjectPath pi -> (pi,undefined,undefined)
    projectMatches = searchTreeForProject pi tree
    moduleMatches = flip concatMap projectMatches 
        $ \(projectIndex, projectTree) -> flip map (searchTreeForModule mi $ projectTree)
            $ \(moduleIndices, moduleTree) -> (projectIndex : moduleIndices, moduleTree)
    declMatches = flip concatMap moduleMatches 
        $ \(moduleIndices, moduleTree) -> flip map (searchTreeForDeclaration di $ moduleTree)
            $ \(declarationIndex, declarationTree) -> (moduleIndices ++ [declarationIndex], declarationTree)

searchTreeForModulePart :: ModuleInfo -> Tree SolutionTreeElem -> [(Int, Tree SolutionTreeElem)]
searchTreeForModulePart (ModuleInfo (Symbol s)) = searchTree''' moduleNameIsPrefixOf
  where
    moduleNameIsPrefixOf (ModuleElem (ModuleInfo (Symbol s')) _) = s' `isPrefixOf` s && case drop (length s') s of
        [] -> True
        ('.':_) -> True
        _ -> False
    moduleNameIsPrefixOf _ = False

searchTreeForModule :: ModuleInfo -> Tree SolutionTreeElem -> [([Int], Tree SolutionTreeElem)]
searchTreeForModule mi tree = case searchTree''' matchesModule tree of
    [] -> let partMatches = searchTreeForModulePart mi tree
          in flip concatMap partMatches $ 
                \(i, nextTree) -> map (\(is,moduleTree) -> (i:is,moduleTree)) 
                             $ searchTreeForModule mi nextTree
    xs -> map (\(i, tree') -> ([i],tree')) xs
  where
    matchesModule (ModuleElem mi' _) = mi == mi'
    matchesModule _ = False

searchTreeForDeclaration :: DeclarationInfo -> Tree SolutionTreeElem -> [(Int, Tree SolutionTreeElem)]
searchTreeForDeclaration di = searchTree''' matchesDeclaration
  where
    matchesDeclaration = ((DeclElem di) ==)

searchTreeForProject :: ProjectInfo -> Tree SolutionTreeElem -> [(Int, Tree SolutionTreeElem)]
searchTreeForProject pi = searchTree''' matchesProject
  where
    matchesProject = ((ProjectElem pi) ==)




searchTree''' :: (SolutionTreeElem -> Bool) -> Tree SolutionTreeElem -> [(Int, Tree SolutionTreeElem)]
searchTree''' f tree = filter (f . rootLabel . snd) $ zip [0..] $ subForest tree

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
    node <- getElemAtSolutionPath path
    --parentNode <- treeStoreGetValue treeStore parentPath
    parentNode <- getElemAtSolutionPath parentPath
    --grandparentNode <- treeStoreGetValue treeStore grandparentPath
    grandparentNode <- getElemAtSolutionPath grandparentPath
    --ancestorNode <- treeStoreGetValue treeStore ancestorPath
    ancestorNode <- getElemAtSolutionPath ancestorPath
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
