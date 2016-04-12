module Digest where

import Data.List
import Data.Maybe

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Except

import System.Posix.Directory
import System.Posix.Files
import System.FilePath.Posix

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
                        childPath -> ((path </> childPath):) <$> loop
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

digestProject' :: MonadIO m => FilePath -> ProjectResult m Project
digestProject' path = do
    contents <- liftIO $ enumerateHaskellProject path
    let fold_f pj (p,c) = do
            (mod,_,_) <- case Module.parse c (Just p) of
                    Right x -> Right x
                    Left msg -> Left $ "Parse error: " ++ msg
            Project.addModule pj mod
    ExceptT $ return $ foldM fold_f Project.empty contents

digestProject :: (MonadIO m, ProjectM m) => FilePath -> ProjectResult m ()
digestProject path = do
    contents <- liftIO $ enumerateHaskellProject path
    forM_ contents $  \(p,c) -> addRawModule c (Just p)
