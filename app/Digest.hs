module Digest where

import Data.List

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Except

import System.Posix.Directory
import System.Posix.Files
import System.FilePath.Posix

import qualified HsInterface as Iface

import Ide3.Types
import Ide3.Monad
import Ide3.Mechanism

import qualified Ide3.Project as Project
import qualified Ide3.Module as Module

data FileTree
    = Directory FilePath [FileTree]
    | File FilePath
    deriving Show
enumerateDirectory :: String -> IO FileTree
enumerateDirectory path = do
    isDir <- isDirectory <$> getFileStatus path
    if not isDir
        then return $ File path
        else do
            stream <- openDirStream path
            let loop = do
                    childPath <- readDirStream stream
                    case childPath of
                        "" -> return []
                        "." -> loop
                        ".." -> loop
                        realPath -> ((path </> realPath):) <$> loop
            paths <- loop
            closeDirStream stream
            branches <- mapM enumerateDirectory paths
            return $ Directory path branches
 
findHaskellFiles :: FileTree -> FileTree
findHaskellFiles (File path)
    | ".hs" `isSuffixOf` path = File path
    | otherwise = File ""
findHaskellFiles (Directory path branches) 
    = Directory path $ filter isHaskellFile $ map findHaskellFiles branches
  where
    isHaskellFile (File x) = not $ null x
    isHaskellFile _ = True

getFilesInTree :: FileTree -> IO [(FilePath,String)]
getFilesInTree (File path) = return <$> (,) path <$> readFile path
getFilesInTree (Directory _ branches) = concat <$> mapM getFilesInTree branches

enumerateHaskellProject :: FilePath -> IO [(FilePath,String)]
enumerateHaskellProject path = do
    fileTree <- enumerateDirectory path
    let haskellTree = findHaskellFiles fileTree
    getFilesInTree haskellTree

foldAddModule :: Project -> (FilePath,String) -> Either (ProjectError u) Project
foldAddModule pj (p,c) = do
    (module_,_,_) <- Module.parse c (Just p)
    Project.addModule pj module_

foldAddExternModule :: Project -> Iface.Interface -> Either (ProjectError u) Project
foldAddExternModule pj i = Project.addExternModule pj (convIface i)
  where
    convExport (Iface.SingleExport s) = SingleExternExport (Symbol s)
    convExport (Iface.MultiExport s ss) = MultiExternExport (Symbol s) (map Symbol ss)
    convIface iface = ExternModule (ModuleInfo $ Symbol $ Iface.modName i) $ case Iface.exports iface of
        Nothing -> []
        Just es -> map convExport es
    

digestProject' :: MonadIO m => FilePath -> ProjectResult m u Project
digestProject' path = do
    contents <- liftIO $ enumerateHaskellProject path
    ifaceFile <- liftIO $ readFile "ifaces" -- TODO: FOR THE LOVE OF GOD FIX THIS SOON
    let ifaces = read ifaceFile :: [Iface.Interface]
    let project = do
            withModules <- foldM foldAddModule Project.empty contents
            withExternModules <- foldM foldAddExternModule withModules ifaces
            return withExternModules
    ExceptT $ return project

digestProject :: (MonadIO m, ProjectM m) => FilePath -> ProjectResult m u ()
digestProject path = do
    contents <- liftIO $ enumerateHaskellProject path
    forM_ contents $  \(p,c) -> addRawModule c (Just p)
