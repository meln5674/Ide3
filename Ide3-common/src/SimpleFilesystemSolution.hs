{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-|
Module      : ReadOnlyFilesystemSolution
Description : Read/Show persistence mechanism
Copyright   : (c) Andrew Melnick, 2016

License     : BSD3
Maintainer  : meln5674@kettering.edu
Stability   : experimental
Portability : POSIX

This modules provides a persistance mechanism which can digest existing projects
as ReadOnlyFilesystemSolution can, but also can read and save projects from a
single file using the Read/Show instances for the Solution type.

-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
module SimpleFilesystemSolution
    ( SimpleFilesystemSolutionT (SimpleFilesystemSolutionT)
    , FileSystemSolution (Unopened)
    , runSimpleFilesystemSolutionT
    ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Data.List
import Data.Maybe

import Text.Read (readMaybe)

import Control.Monad.Catch

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Except
import Control.Monad.Trans.State.Strict

import System.Directory
import System.IO.Error

import System.FilePath

import Ide3.NewMonad hiding (load)
import Ide3.NewMonad.Instances.State.Class
import Ide3.Types
import qualified Ide3.Solution as Solution
import Ide3.Digest
import Ide3.ModuleTree
import Ide3.Utils

import ViewerMonad
import PseudoState


-- | State of the mechanism
data FileSystemSolution
    -- | No solution opened
    = Unopened
    -- | A digested path or dump file is opened if Just, a new project if Nothing
    | Opened (Maybe SimpleFilePath)

data SimpleFilePath = DumpFilePath FilePath | SolutionDirPath FilePath

{-
-- | State transformer for the mechanism
newtype SimpleFilesystemSolutionT' m a
    = SimpleFilesystemSolutionT'
    { runSimpleFilesystemSolutionT'Internal :: StateT FileSystemSolution m a
    }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , SolutionStateM
    )
-}

-- | Type wrapper for using a file as the project store
newtype SimpleFilesystemSolutionT m a
    = SimpleFilesystemSolutionT
    { runSimpleFilesystemSolutionTInternal :: StateT FileSystemSolution m a
    }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    )

instance MonadBounce SimpleFilesystemSolutionT where
    bounce = ExceptT . SimpleFilesystemSolutionT . lift . runExceptT

{-
instance MonadTrans SimpleFilesystemSolutionT' where
    lift = SimpleFilesystemSolutionT' . lift
-}

instance MonadTrans SimpleFilesystemSolutionT where
    lift = SimpleFilesystemSolutionT . lift

--deriving instance (MonadMask m) => MonadMask (SimpleFilesystemSolutionT' m)
deriving instance (MonadMask m) => MonadMask (SimpleFilesystemSolutionT m)
--deriving instance (MonadCatch m) => MonadCatch (SimpleFilesystemSolutionT' m)
deriving instance (MonadCatch m) => MonadCatch (SimpleFilesystemSolutionT m)
--deriving instance (MonadThrow m) => MonadThrow (SimpleFilesystemSolutionT' m)
deriving instance (MonadThrow m) => MonadThrow (SimpleFilesystemSolutionT m)

-- | Run an action inside the mechanism with the provided state
runSimpleFilesystemSolutionT :: SimpleFilesystemSolutionT m a -> FileSystemSolution -> m (a, FileSystemSolution)
runSimpleFilesystemSolutionT 
    = runStateT 
--    . runSimpleFilesystemSolutionT'Internal 
--    . runStatefulSolution 
    . runSimpleFilesystemSolutionTInternal

-- | Run an action inside the mechanism 
--runNewSimpleFilesystemSolutionT :: SimpleFilesystemSolutionT m a -> m (a, FileSystemSolution)
--runNewSimpleFilesystemSolutionT = flip runSimpleFilesystemSolutionT Unopened

-- | Get the project state from the outer type
getFsp :: (Monad m) => SimpleFilesystemSolutionT m FileSystemSolution
--getFsp = SimpleFilesystemSolutionT $ lift $ getFsp'
getFsp = SimpleFilesystemSolutionT get

-- | Set the project state from the outer type
putFsp :: (Monad m) => FileSystemSolution -> SimpleFilesystemSolutionT m ()
--putFsp = SimpleFilesystemSolutionT . lift . putFsp'
putFsp = SimpleFilesystemSolutionT . put

{-
instance SolutionStateM m => SolutionStateM (SimpleFilesystemSolutionT m) where
    getSolution = lift getSolution
    putSolution = lift . putSolution
-}

--instance MonadIO m => SolutionShellM (SimpleFilesystemSolutionT' m) where
instance MonadIO m => StatefulPersistenceClass (SimpleFilesystemSolutionT m) where
    type StatefulPersistToken (SimpleFilesystemSolutionT m) = SimpleFilePath
    -- | Create a new project
    newState i = do
        lift $ putFsp $ Opened Nothing
        return $ Solution.new i
    -- | Either digest a directory or open a file and use Read on the contents
    loadState pathTok = case pathTok of
        DumpFilePath path -> do
            result <- liftIO $ tryIOError $ readFile path
            case result of
                Right contents -> case readMaybe contents of
                    Just p -> do
                        lift $ putFsp $ Opened $ Just pathTok
                        return p
                    Nothing -> throwE $ InvalidOperation "File did not contain a valid project" ""
                Left err -> throwE $ InvalidOperation ("Error on opening file: " ++ show err) ""
        SolutionDirPath solutionPath -> do
            let parts = splitPath solutionPath
                projectName = T.pack $ last parts
                solutionName = T.pack $ last parts
                project = Params
                          ( ProjectInfo projectName )
                            solutionPath
                          ( Just $ solutionPath </> "ifaces" )
            p <- digestSolution (SolutionInfo solutionName) [project]
            lift $ putFsp $ Opened $ Just pathTok
            return p
    -- | Use Show to turn the current project into a string and write it to the
    -- correct file
    finalizeState Nothing p = do
        fsp <- lift getFsp
        case fsp of
            Opened (Just path) -> finalizeState (Just path) p
            _ -> throwE $ InvalidOperation "No open solution" ""
    finalizeState (Just (DumpFilePath path)) p = do
        fsp <- lift getFsp
        case fsp of
            Opened _ -> do
                result <- liftIO $ tryIOError $ writeFile path $ show p
                lift $ putFsp $ Opened $ Just $ DumpFilePath path
                case result of
                    Right _ -> return ()
                    Left err -> throwE $ InvalidOperation ("Error on writing file: " ++ show err) ""
            _ -> throwE $ InvalidOperation "No open solution" ""
    finalizeState (Just (SolutionDirPath path)) p = 
        throwE $ InvalidOperation "Directory solutions are read-only" ""

-- | A pair of filename and file contents to write to disc
data OutputPair
    = OutputPair
    { filePath :: FilePath
    , fileContents :: Text
    }

-- | A list of directories to create and files to write
data FileListing
    = FileListing
    { directoriesNeeded :: [FilePath]
    , outputs :: [OutputPair]
    }

-- | Make a listing of files to write out
makeFileListing :: ( ProjectModuleClass m
                   , ModuleExportClass m
                   , ModuleImportClass m
                   , ModuleDeclarationClass m
                   , ModulePragmaClass m
                   ) 
                => ProjectInfo 
                -> SolutionResult u m FileListing
makeFileListing pji = do
    t <- makeTree pji
    let subModules (OrgNode _ ts) = ts
        subModules (ModuleNode _ ts _ _ _ _) = ts
        subModules (UnparsableModuleNode _ ts _ _ _) = ts
        
        nodeInfo (OrgNode mi _) = mi
        nodeInfo (ModuleNode mi _ _ _ _ _) = mi
        nodeInfo (UnparsableModuleNode mi _ _ _ _) = mi
        
        nodeDecls (ModuleNode _ _ _ ds _ _) = Just ds
        nodeDecls _ = Nothing
        
        dirs node = (takeDirectory $ moduleInfoToPath "" $ nodeInfo node) : subDirs node
        subDirs node = concatMap dirs $ subModules node
        
        {-
        dirs (OrgNode i ts) = (: (concat $ mapMaybe dirs ts)) <$> (takeDirectory <$> modulePath i)
        dirs (ModuleNode i ts _ _ _ _) = (: (concat $ mapMaybe dirs ts)) <$> (takeDirectory <$> modulePath i)
        -}
        declInfos node = case nodeDecls node of
            Just ds -> (nodeInfo node, ds) : subDecls
            Nothing -> subDecls
          where
            subDecls = concatMap declInfos $ subModules node
        {-
        declInfos (OrgNode _ ts) = concatMap declInfos ts
        declInfos (ModuleNode i ts _ ds _ _) = (i, ds) : concatMap declInfos ts
        declInfos (UnparsableModuleNode
        -}
        dirs' = concatMap dirs t
        declInfos' = concatMap declInfos t
    decls <- forM declInfos' $ \(mi, dis) -> do
        ds <- forM dis $ getDeclaration pji mi
        return $ OutputPair (moduleInfoToPath "" mi) $ T.intercalate "\n" $ map body ds
    return FileListing
           { directoriesNeeded = dirs'
           , outputs = decls
           }

-- | Write an output pair to disc
writeOutputPair :: (MonadIO m) => OutputPair -> m ()
writeOutputPair pair = liftIO $ T.writeFile (filePath pair) (fileContents pair)

-- | Create the directories needed and write the files to be written
executeFileListing :: (MonadIO m) => FileListing -> SolutionResult u m ()
executeFileListing listing = wrapIOError $ do
    mapM_ (createDirectoryIfMissing True) $ directoriesNeeded listing
    mapM_ writeOutputPair $ outputs listing

instance ( MonadIO m
         , SolutionClass m
         , PersistenceClass m
         , ProjectModuleClass m
         , ModuleExportClass m
         , ModuleImportClass m
         , ModuleDeclarationClass m
         , ModulePragmaClass m
         ) 
         => ViewerMonad (SimpleFilesystemSolutionT m) where
    type ViewerPersistToken (SimpleFilesystemSolutionT m) = SimpleFilePath
    preparePathToken path = do
        isDir <- liftIO $ doesDirectoryExist path
        if isDir
            then return $ SolutionDirPath path
            else return $ DumpFilePath path
{-
    -- | Set the Read file to be opened
    setFileToOpen path = lift $ putFsp $ ToOpen path
    -- | Set the path to be digested
    setDirectoryToOpen path = lift $ putFsp $ ToDigest path
    -- | Set the path to Show to
    setTargetPath path = do
        fsp <- lift getFsp
        case fsp of
            Opened (Just _) -> lift $ putFsp $ Opened $ Just path
            Opened Nothing -> do
                lift $ putFsp $ Opened $ Just path
            _ -> throwE $ InvalidOperation "Cannot set target path without open project" ""
-}
    -- | Check if either there is a new project, digested path, or Read'd file
    hasOpenedSolution = do
        fsp <- getFsp
        case fsp of
            Opened _ -> return True
            _ -> return False
{-
    -- | Create a new file project
    createNewFile path = do
        let solutionName = takeBaseName path
        bounce $ new $ SolutionInfo solutionName
        lift $ putFsp $ Opened Nothing
        setTargetPath path
    -- | Unsupported
    createNewDirectory _ = throwE $ Unsupported "Cannot create a directory project using simple"
-}    
    -- | Get a list of files and directories needed and then create them
    prepareBuild = bounce $ getProjects >>= mapM_ (makeFileListing >=> executeFileListing)


instance PseudoStateT SimpleFilesystemSolutionT FileSystemSolution where
--    runPseudoStateT = runStateT . runSimpleFilesystemSolutionT'Internal . runStatefulSolution . runSimpleFilesystemSolutionTInternal
    runPseudoStateT = runStateT . runSimpleFilesystemSolutionTInternal


