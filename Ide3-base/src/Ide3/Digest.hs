{-|
Module      : Ide3.Constructor
Description : Digesting projects
Copyright   : (c) Andrew Melnick, 2016

License     : BSD3
Maintainer  : meln5674@kettering.edu
Stability   : experimental
Portability : POSIX

Provides functions which can enumerate the haskell source files in a project
tree and construct a project from them
-}
module Ide3.Digest
    ( digestProject
    , digestProject'
    ) where

import Data.List

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Except

import System.Posix.Directory
import System.Posix.Files
import System.FilePath

import qualified HsInterface as Iface

import Ide3.Types
import Ide3.Monad
import Ide3.Mechanism

import qualified Ide3.Project as Project
import qualified Ide3.Module as Module

-- | Represents a simplified directory structure
data FileTree
    = Directory FilePath [FileTree]
    | File FilePath
    deriving Show

-- | Take a file path and get the structure of the files and directories beneath it
enumerateDirectory :: FilePath -> IO FileTree
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
 
 -- | Take a file tree and prune any files which are not haskell source files
findHaskellFiles :: FileTree -> FileTree
findHaskellFiles (File path)
    | ".hs" `isSuffixOf` path = File path
    | otherwise = File ""
findHaskellFiles (Directory path branches) 
    = Directory path $ filter isHaskellFile $ map findHaskellFiles branches
  where
    isHaskellFile (File x) = not $ null x
    isHaskellFile _ = True

-- | Take a file tree and read each of the files in it, returning a pair of the
-- file's path as well as its contents
getFilesInTree :: FileTree -> IO [(FilePath,String)]
getFilesInTree (File path) = return . (,) path <$> readFile path
getFilesInTree (Directory _ branches) = concat <$> mapM getFilesInTree branches

-- | Take a path to a directory, and return a list of pairs of file paths and
-- file contents for each haskell source file in that directory
enumerateHaskellProject :: FilePath -> IO [(FilePath,String)]
enumerateHaskellProject path = do
    fileTree <- enumerateDirectory path
    let haskellTree = findHaskellFiles fileTree
    getFilesInTree haskellTree

-- | Parse and add a module to a project
foldAddModule :: Project -> (FilePath,String) -> Either (ProjectError u) Project
foldAddModule pj (p,c) = do
    (module_,_,_) <- Module.parse c (Just p)
    Project.addModule pj module_

-- | Add an interface to a project as an external module
foldAddExternModule :: Project -> Iface.Interface -> Either (ProjectError u) Project
foldAddExternModule pj i = Project.addExternModule pj (convIface i)
  where
    convExport (Iface.SingleExport s) = SingleExternExport (Symbol s)
    convExport (Iface.MultiExport s ss) = MultiExternExport (Symbol s) (map Symbol ss)
    convIface iface = ExternModule (ModuleInfo $ Symbol $ Iface.modName i) $ case Iface.exports iface of
        Nothing -> []
        Just es -> map convExport es
    

-- | Take a path to a directory and opitonally an interface file and create a new
-- project structure from the haskell files in it
digestProject' :: MonadIO m => FilePath -> Maybe FilePath -> ProjectResult m u Project
digestProject' path maybeIfacePath = do
    contents <- liftIO $ enumerateHaskellProject path
    withoutIfaces <- ExceptT $ return $ foldM foldAddModule Project.empty contents
    case maybeIfacePath of
        Nothing -> return withoutIfaces
        Just ifacePath -> do
            ifaceFile <- liftIO $ readFile ifacePath 
            let ifaces = read ifaceFile :: [Iface.Interface]
            ExceptT $ return $ foldM foldAddExternModule withoutIfaces ifaces

-- | Take a path to a directory and the modules there to the current project
digestProject :: (MonadIO m, ProjectM m) => FilePath -> ProjectResult m u ()
digestProject path = do
    contents <- liftIO $ enumerateHaskellProject path
    forM_ contents $  \(p,c) -> addRawModule c (Just p)
