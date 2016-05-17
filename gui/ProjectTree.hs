{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module ProjectTree where

import Data.Tree

import Control.Monad.Trans
import Control.Monad.Trans.Except

import Ide3.Types
import Ide3.ModuleTree
import Ide3.Monad
import Ide3.Mechanism.State

import Graphics.UI.Gtk

import ViewerMonad

data ProjectTreeElem
    = ModuleElem ModuleInfo
    | DeclElem DeclarationInfo

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

makeProjectTree :: ModuleTree -> Tree ProjectTreeElem
makeProjectTree (OrgNode mi ts) = Node (ModuleElem mi) $ map makeProjectTree ts
makeProjectTree (ModuleNode mi ds ts) = Node (ModuleElem mi) $ map makeProjectTree ts ++ map (flip Node [] . DeclElem) ds 
    

populateTree :: (MonadIO m, ViewerMonad m) => TreeStore ProjectTreeElem -> ProjectResult m u ()
populateTree treeStore = do
    trees <- makeTree 
    let trees' = map makeProjectTree trees
    liftIO $ do
        treeStoreClear treeStore
        treeStoreInsertForest treeStore [] 0 trees'
    
getModuleAndDecl :: TreePath -> TreeStore ProjectTreeElem -> IO (Maybe (ModuleInfo,DeclarationInfo))
getModuleAndDecl path treeStore = do
    node <- treeStoreGetValue treeStore path 
    case node of
        ModuleElem _ -> return Nothing
        DeclElem di -> do
            let parentPath = init path
            parentNode <- treeStoreGetValue treeStore parentPath
            case parentNode of
                ModuleElem mi -> return $ Just (mi,di)
                _ -> return Nothing 
