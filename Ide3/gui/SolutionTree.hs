module SolutionTree where

import Data.Tree

import Control.Monad
import Control.Monad.Trans

import Ide3.Types
import Ide3.ModuleTree
import Ide3.Monad

import Graphics.UI.Gtk

import ViewerMonad

data SolutionTreeElem
    = ProjectElem ProjectInfo
    | ModuleElem ModuleInfo
    | DeclElem DeclarationInfo
    | ImportsElem
    | ExportsElem
    | PragmasElem
    | ImportElem ImportId (WithBody Import)
    | ExportElem ExportId (WithBody Export)
    | PragmaElem Pragma

data TreeSearchResult
    = ProjectResult ProjectInfo
    | ModuleResult ProjectInfo ModuleInfo
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

renderSolutionTreeElem :: CellRendererTextClass o => SolutionTreeElem -> [AttrOp o]
renderSolutionTreeElem (ProjectElem (ProjectInfo n)) = [cellText := n]
renderSolutionTreeElem (ModuleElem (ModuleInfo (Symbol s))) = [cellText := s]
renderSolutionTreeElem (ModuleElem (UnamedModule (Just path))) = [cellText := path]
renderSolutionTreeElem (ModuleElem (UnamedModule Nothing)) = [cellText := "???"]
renderSolutionTreeElem (DeclElem (DeclarationInfo (Symbol s))) = [cellText := s]
renderSolutionTreeElem ImportsElem = [cellText := "Imports"]
renderSolutionTreeElem ExportsElem = [cellText := "Exports"]
renderSolutionTreeElem PragmasElem = [cellText := "Pragmas"]
renderSolutionTreeElem (ImportElem _ (WithBody _ importBody)) = [cellText := importBody] 
renderSolutionTreeElem (ExportElem _ (WithBody _ exportBody)) = [cellText := exportBody] 
renderSolutionTreeElem (PragmaElem p) = [cellText := p]

makePragmasNode :: [Pragma] -> Tree SolutionTreeElem
makePragmasNode ps = Node PragmasElem $ map (flip Node [] . PragmaElem) ps

makeImportsNode :: [(ImportId,WithBody Import)] -> Tree SolutionTreeElem
makeImportsNode is = Node ImportsElem $ map (flip Node [] . uncurry ImportElem) is

makeExportsNode :: Maybe [(ExportId,WithBody Export)] -> Tree SolutionTreeElem
makeExportsNode (Just es) = Node ExportsElem $ map (flip Node [] . uncurry ExportElem) es
makeExportsNode Nothing = Node ExportsElem []

makeModuleTree :: ModuleTree -> Tree SolutionTreeElem
makeModuleTree (OrgNode mi ts)
    = Node (ModuleElem mi) $ map makeModuleTree ts
makeModuleTree (ModuleNode mi ts ps ds is es) 
    = Node (ModuleElem mi) 
    $ makePragmasNode ps
    : makeImportsNode is 
    : makeExportsNode es 
    : map makeModuleTree ts 
    ++ map (flip Node [] . DeclElem) ds 
    
makeProjectTree :: ProjectInfo -> [ModuleTree] -> Tree SolutionTreeElem
makeProjectTree pi branches = Node (ProjectElem pi) $ map makeModuleTree branches

makeSolutionTree :: [(ProjectInfo,[ModuleTree])] -> [Tree SolutionTreeElem]
makeSolutionTree = map $ uncurry makeProjectTree

populateTree :: (MonadIO m, ViewerMonad m) => TreeStore SolutionTreeElem -> SolutionResult m u ()
populateTree treeStore = do
    projects <- getProjects
    trees <- forM projects $ \pi -> do
        tree <- makeTree pi
        return (pi, tree)
    let forest = makeSolutionTree trees
    liftIO $ do
        treeStoreClear treeStore
        treeStoreInsertForest treeStore [] 0 forest

findAtPath :: TreePath -> TreeStore SolutionTreeElem -> IO TreeSearchResult
findAtPath path treeStore = do
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
    node <- treeStoreGetValue treeStore path
    parentNode <- treeStoreGetValue treeStore parentPath
    grandparentNode <- treeStoreGetValue treeStore grandparentPath
    ancestorNode <- treeStoreGetValue treeStore ancestorPath
    case (node,parentNode,grandparentNode,ancestorNode) of
        (ProjectElem pi,_,_,_) -> return $ ProjectResult pi
        (ModuleElem mi,_,_,ProjectElem pi) -> return $ ModuleResult pi mi
        (DeclElem di,ModuleElem mi,_,ProjectElem pi) -> return $ DeclResult pi mi di
        (ImportsElem,ModuleElem mi,_,ProjectElem pi) -> return $ ImportsResult pi mi
        (ExportsElem,ModuleElem mi,_,ProjectElem pi) -> return $ ExportsResult pi mi
        (ImportElem ii _,_,ModuleElem mi,ProjectElem pi) -> return $ ImportResult pi mi ii
        (ExportElem ei _,_,ModuleElem mi,ProjectElem pi) -> return $ ExportResult pi mi ei
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
