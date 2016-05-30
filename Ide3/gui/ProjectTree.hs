module ProjectTree where

import Data.Tree

import Control.Monad.Trans

import Ide3.Types
import Ide3.ModuleTree
import Ide3.Monad

import Graphics.UI.Gtk

import ViewerMonad

data ProjectTreeElem
    = ModuleElem ModuleInfo
    | DeclElem DeclarationInfo
    | ImportsElem
    | ExportsElem
    | ImportElem ImportId (WithBody Import)
    | ExportElem ExportId (WithBody Export)

data TreeSearchResult
    = ModuleResult ModuleInfo
    | DeclResult ModuleInfo DeclarationInfo
    | ImportsResult ModuleInfo
    | ExportsResult ModuleInfo
    | ImportResult ModuleInfo ImportId
    | ExportResult ModuleInfo ExportId
    | NoSearchResult

{-
newtype PSW a = PSW { unPSW :: ProjectStateT IO a }
    deriving (Functor,Applicative,Monad,ProjectStateM,MonadIO)

instance ProjectShellM PSW where
    load = error "STOP"
    new = error "STOP"
    finalize = error "STOP"
-}

renderProjectTreeElem :: CellRendererTextClass o => ProjectTreeElem -> [AttrOp o]
renderProjectTreeElem (ModuleElem (ModuleInfo (Symbol s))) = [cellText := s]
renderProjectTreeElem (ModuleElem (UnamedModule (Just path))) = [cellText := path]
renderProjectTreeElem (ModuleElem (UnamedModule Nothing)) = [cellText := "???"]
renderProjectTreeElem (DeclElem (DeclarationInfo (Symbol s))) = [cellText := s]
renderProjectTreeElem ImportsElem = [cellText := "Imports"]
renderProjectTreeElem ExportsElem = [cellText := "Exports"]
renderProjectTreeElem (ImportElem _ (WithBody _ importBody)) = [cellText := importBody] 
renderProjectTreeElem (ExportElem _ (WithBody _ exportBody)) = [cellText := exportBody] 

makeImportsNode :: [(ImportId,WithBody Import)] -> Tree ProjectTreeElem
makeImportsNode is = Node ImportsElem $ map (flip Node [] . uncurry ImportElem) is

makeExportsNode :: Maybe [(ExportId,WithBody Export)] -> Tree ProjectTreeElem
makeExportsNode (Just es) = Node ExportsElem $ map (flip Node [] . uncurry ExportElem) es
makeExportsNode Nothing = Node ExportsElem []

makeProjectTree :: ModuleTree -> Tree ProjectTreeElem
makeProjectTree (OrgNode mi ts)
    = Node (ModuleElem mi) $ map makeProjectTree ts
makeProjectTree (ModuleNode mi ds ts is es) 
    = Node (ModuleElem mi) 
    $ makeImportsNode is 
    : makeExportsNode es 
    : map makeProjectTree ts 
    ++ map (flip Node [] . DeclElem) ds 
    

populateTree :: (MonadIO m, ViewerMonad m) => TreeStore ProjectTreeElem -> ProjectResult m u ()
populateTree treeStore = do
    trees <- makeTree 
    let trees' = map makeProjectTree trees
    liftIO $ do
        treeStoreClear treeStore
        treeStoreInsertForest treeStore [] 0 trees'

findAtPath :: TreePath -> TreeStore ProjectTreeElem -> IO TreeSearchResult
findAtPath path treeStore = do
    let parentPath = case path of
            [] -> []
            _ -> init path
        grandparentPath = case path of
            [] -> []
            [x] -> [x]
            _ -> init $ init path
    node <- treeStoreGetValue treeStore path
    parentNode <- treeStoreGetValue treeStore parentPath
    grandparentNode <- treeStoreGetValue treeStore grandparentPath
    case (node, parentNode,grandparentNode) of
        (ModuleElem mi,_,_) -> return $ ModuleResult mi
        (DeclElem di,ModuleElem mi,_) -> return $ DeclResult mi di
        (ImportsElem,ModuleElem mi,_) -> return $ ImportsResult mi
        (ExportsElem,ModuleElem mi,_) -> return $ ExportsResult mi
        (ImportElem ii _,_,ModuleElem mi) -> return $ ImportResult mi ii
        (ExportElem ei _,_,ModuleElem mi) -> return $ ExportResult mi ei
        _ -> return NoSearchResult

{-
getModuleAndDecl :: TreePath -> TreeStore ProjectTreeElem -> IO (Maybe (ModuleInfo,DeclarationInfo))
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

getModuleAndImport :: TreePath -> TreeStore ProjectTreeElem -> IO (Maybe (ModuleInfo,ImportId))
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

getModuleAndExport :: TreePath -> TreeStore ProjectTreeElem -> IO (Maybe (ModuleInfo,ExportId))
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
