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

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Ide3.Digest
    ( digestSolutionM
    , digestSolution
    ) where

import Data.List

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Except
import Control.Monad.Trans.State

import System.Posix.Directory
import System.Posix.Files
import System.FilePath

import qualified HsInterface as Iface

import Ide3.Types
import Ide3.Monad
import Ide3.Mechanism
import Ide3.Mechanism.State

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

-- | Digest an interface and add an external module accordingly
digestInterfaceM :: (MonadIO m, SolutionM m)
                => ProjectInfo
                -> Iface.Interface
                -> SolutionResult m u ()
digestInterfaceM pji iface = addExternModule pji newModule
  where
    newModule = ExternModule newInfo exports
    newInfo = ModuleInfo $ Symbol $ Iface.modName iface
    exports = case Iface.exports iface of
        Nothing -> []
        Just es -> flip map es $ \case
            Iface.SingleExport s -> SingleExternExport $ Symbol s
            Iface.MultiExport s ss -> MultiExternExport (Symbol s) $ map Symbol ss

-- | Digest a project from a directory structure, optionally providing a path to
-- an interface file for external modules
digestProjectM :: (MonadIO m, SolutionM m) 
                            => ProjectInfo 
                            -> FilePath 
                            -> Maybe FilePath 
                            -> SolutionResult m u ()
digestProjectM pji p ip = do
    contents <- liftIO $ enumerateHaskellProject p
    addProject pji
    forM_ contents $ \(mp,mc) -> addRawModule pji mc (Just mp)
    case ip of
        Nothing -> return ()
        Just ip -> do
            ifaceFile <- liftIO $ readFile ip
            mapM_ (digestInterfaceM pji) $ (read ifaceFile :: [Iface.Interface])


digestSolutionM :: (MonadIO m, SolutionM m)
                => SolutionInfo
                -> FilePath
                -> [(ProjectInfo,FilePath,Maybe FilePath)]
                -> SolutionResult m u ()
digestSolutionM si p ps = do
    editSolutionInfo (const $ si)
    forM_ ps $ \(pji,pp,ip) -> digestProjectM pji pp ip

newtype Wrapper m a = Wrapper { runWrapper :: m a }
  deriving (Functor, Applicative, Monad, MonadIO, SolutionStateM)

instance MonadTrans Wrapper where
    lift = Wrapper
  
instance Monad m => SolutionShellM (Wrapper m) where
    load = error "IDIOT"
    new = error "IDIOT"
    finalize = error "IDIOT"

digestSolution :: forall m u
                . (MonadIO m) 
               => SolutionInfo
               -> FilePath
               -> [(ProjectInfo,FilePath,Maybe FilePath)]
               -> SolutionResult m u Solution
digestSolution si p ps = do
    let y :: MonadIO m => SolutionResult (StatefulSolution (Wrapper (SolutionStateT m))) u Solution
        y = do
            digestSolutionM si p ps
            lift $ getSolution
    (z,_) <- lift $ runNewSolutionStateT $ runWrapper $ runStatefulSolution $ runExceptT y
    ExceptT $ return z
