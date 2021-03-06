{-|
Module      : Ide3.Digest
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
{-# LANGUAGE OverloadedStrings #-}
module Ide3.Digest
    ( digestSolutionM
    , digestSolution
    , ProjectDigestParams (..)
    ) where

import qualified Data.Text as T

import Data.List

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.State
import Control.Monad.Trans.Except

import System.Posix.Directory
import System.Posix.Files
import System.FilePath

import qualified HsInterface as Iface

import Ide3.Utils

import Ide3.Types.Internal
import Ide3.Types.State
import Ide3.NewMonad
import Ide3.NewMonad.Utils

import qualified Ide3.Solution as Solution

import Ide3.NewMonad.Instances.State()
import Ide3.NewMonad.Instances.State.Class
import Ide3.NewMonad.Instances.State.Class.Instances.Lazy

-- | Represents a simplified directory structure
data FileTree
    = Directory FilePath [FileTree]
    | File FilePath
    deriving Show

-- | Take a file path and get the structure of the files and directories beneath
-- it
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
digestInterfaceM :: ( MonadIO m
                    , ProjectExternModuleClass m
                    , ExternModuleExportClass m
                    )
                 => ProjectInfo
                 -> Iface.Interface
                 -> SolutionResult u m ()
digestInterfaceM pji iface = do
    createExternModule pji newInfo
    forM_ exports $ addExternExport pji newInfo
  where
    newInfo = ModuleInfo $ Symbol $ T.pack $ Iface.modName iface
    exports = case Iface.exports iface of
        Nothing -> []
        Just es -> flip map es $ \case
            Iface.SingleExport s -> SingleExternExport $ Symbol $ T.pack s
            Iface.MultiExport s ss
                -> MultiExternExport (Symbol $ T.pack s) $ map (Symbol . T.pack) ss

-- | Digest a project from a directory structure, optionally providing a path to
-- an interface file for external modules
digestProjectM :: ( MonadIO m
                  , SolutionClass m
                  , ProjectModuleClass m
                  , ProjectExternModuleClass m
                  , ExternModuleExportClass m
                  , ModulePragmaClass m
                  , ModuleImportClass m
                  , ModuleExportClass m
                  , ModuleDeclarationClass m
                  )
               => ProjectInfo 
               -> FilePath 
               -> Maybe FilePath 
               -> SolutionResult u m ()
digestProjectM pji proot maybeip = do
    contents <- liftIO $ enumerateHaskellProject proot
    addProject pji
    forM_ contents $ \(mp,mc) -> do
        addRawModule pji mc (Just mp) (Just $ pathToModuleInfo proot mp)
    case maybeip of
        Nothing -> return ()
        Just ip -> do
            ifaceFile <- liftIO $ readFile ip
            mapM_ (digestInterfaceM pji) (read ifaceFile :: [Iface.Interface])

-- | Parameters for digesting a project
data ProjectDigestParams 
    = Params 
    { projectDigestInfo :: ProjectInfo 
    , projectDigestRoot :: FilePath 
    , projectDigestInterfacePath :: Maybe FilePath 
    } 

-- | Digest a solution from the file system inside of a monad transformer
digestSolutionM :: ( MonadIO m
                   , SolutionClass m
                   , ProjectModuleClass m
                   , ProjectExternModuleClass m
                   , ExternModuleExportClass m
                   , ModulePragmaClass m
                   , ModuleImportClass m
                   , ModuleExportClass m
                   , ModuleDeclarationClass m
                   )
                => SolutionInfo 
                -> [ProjectDigestParams]
                -> SolutionResult u m ()
digestSolutionM si ps = do
    editSolutionInfo $ const si
    forM_ ps $ \(Params pji pp ip) -> digestProjectM pji pp ip

-- | Digest a solution from the file system
digestSolution :: forall m u
                . ( MonadIO m )
               => SolutionInfo
               -> [ProjectDigestParams]
               -> SolutionResult u m Solution
digestSolution si ps = do
    let y :: SolutionResult u (StatefulWrapper (SolutionStateT m)) Solution
        y = digestSolutionM si ps >> getSolution
    (z,_) <- lift $ flip runStateT Solution.empty 
                  $ runSolutionStateT 
                  $ runStatefulWrapper 
                  $ runExceptT y
    ExceptT $ return z

