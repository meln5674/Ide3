{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-|
Module      : CabalFilesystemProject
Description : Persistance mechanism using .cabal files
Copyright   : (c) Andrew Melnick, 2016

License     : BSD3
Maintainer  : meln5674@kettering.edu
Stability   : experimental
Portability : POSIX

The Cabal persistance mechanism uses .cabal files to store the list of modules
in a project, and stores declarations in the module files as normal
-}
module CabalFilesystemProject
    ( CabalProject(CabalProject)
    , FileSystemProject (Unopened)
    , runCabalProject
    ) where

import Data.List

import System.Directory
import System.FilePath

import Control.Monad.Catch

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Except
import Control.Monad.Trans.State.Strict

import qualified Distribution.ModuleName as ModuleName
import Distribution.Verbosity
import Distribution.PackageDescription
import Distribution.PackageDescription.Parse
import Distribution.PackageDescription.Configuration
import Distribution.PackageDescription.PrettyPrint

import Ide3.Types
import Ide3.Utils
import Ide3.Monad
import Ide3.Mechanism.State hiding (new, load, finalize)

import qualified Ide3.Project as Project
import qualified Ide3.Module as Module

import ViewerMonad
import PseudoState

-- | State of the mechanism
data FileSystemProject
    -- | A file containing a dump of a Project is to be opened
    = ToOpen FilePath
    -- | No project opened
    | Unopened
    -- | A digested path or dump file is opened if Just, a new project if Nothing
    | Opened (Maybe CabalProjectInfo)

-- | Path to a cabal directory and the configuration
data CabalProjectInfo = CabalProjectInfo FilePath CabalConfiguration

-- | Wrapper around the cabal project configuration
newtype CabalConfiguration = CabalConfiguration GenericPackageDescription

-- | Type wrapper for using a cabal file as a project setup and the module
-- files to store code normally
newtype CabalProject m a
    = CabalProject { runCabalProjectInternal :: StateT FileSystemProject m a }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadTrans
    , MonadIO
    , ProjectStateM
    )

-- | Run a cabal project
runCabalProject :: CabalProject m a -> FileSystemProject -> m (a, FileSystemProject)
runCabalProject = runStateT . runCabalProjectInternal

deriving instance (MonadMask m) => MonadMask (CabalProject m)
deriving instance (MonadCatch m) => MonadCatch (CabalProject m)
deriving instance (MonadThrow m) => MonadThrow (CabalProject m)

-- | Get the state of the project
getFsp :: (Monad m) => CabalProject m FileSystemProject
getFsp = CabalProject get

-- | Set the state of the project
putFsp :: (Monad m) => FileSystemProject -> CabalProject m ()
putFsp = CabalProject . put 

-- | Perform an action on an open project, or throw an error if not open
withOpenedProject :: (Monad m) 
                  => (FilePath -> CabalConfiguration -> ProjectResult (CabalProject m) u a)
                  -> ProjectResult (CabalProject m) u a
withOpenedProject f = do
    fsp <- lift getFsp
    case fsp of
        Opened (Just (CabalProjectInfo path config)) -> f path config
        _ -> throwE $ InvalidOperation "No open project" ""

-- | Update a project's library
updateLibrary :: GenericPackageDescription -> Library -> GenericPackageDescription
updateLibrary desc@GenericPackageDescription{condLibrary=Just condLib} lib 
    = desc{condLibrary=Just condLib{condTreeData = lib}}

-- Update a project's executable
updateExecutable :: GenericPackageDescription -> Executable -> GenericPackageDescription
updateExecutable desc@GenericPackageDescription{condExecutables=[(str,condExe)]} exe
    = desc{condExecutables=[(str,condExe{condTreeData=exe})]}

-- | Update the project configuration
updateConfig :: (Monad m) => ProjectType -> ProjectResult (CabalProject m) u ()
updateConfig (LibraryProject lib) = withOpenedProject $ \path (CabalConfiguration desc) -> do
    let desc' = updateLibrary desc lib
    lift $ putFsp $ Opened $ Just $ CabalProjectInfo path $ CabalConfiguration desc'
updateConfig (ExecutableProject exe) = withOpenedProject $ \path (CabalConfiguration desc) -> do
    let desc' = updateExecutable desc exe
    lift $ putFsp $ Opened $ Just $ CabalProjectInfo path $ CabalConfiguration desc'

-- | Get a list of modules from a cabal project
getInternalModules :: (MonadIO m) => ProjectResult (CabalProject m) u [Module]
getInternalModules = do
    ptype <- getProjectType
    root <- getProjectSourceRoot
    let (mainModuleName,others) = case ptype of
            LibraryProject lib -> (Nothing, exposedModules lib)
            ExecutableProject exe ->
                let others = otherModules $ buildInfo exe
                    mainModule = ModuleName.fromString $ takeBaseName $ modulePath exe
                in (Just mainModule, others)
    let modulePaths = map ((root </>) . (++".hs") . ModuleName.toFilePath) others
    let mainModulePath = liftM ((root </>) . (++".hs") . ModuleName.toFilePath) mainModuleName
    moduleContents <- forM modulePaths $ \path -> do
        contents <- wrapReadFile path
        return (path,contents)
    moduleParses <- forM moduleContents $ \(path,contents) ->
        ExceptT $ return $ Module.parse contents $ Just path
    case mainModulePath of
        Just x -> do
            mainModuleContents <- wrapReadFile x
            (mainModule,_,_) <- ExceptT $ return $ Module.parseMain mainModuleContents $ Just x
            return $ mainModule : map (\(x,_,_) -> x) moduleParses
        Nothing -> return $ map (\(x,_,_) -> x) moduleParses
    
    
-- | Get the external modules used by a cabal project. * INCOMPLETE *
getExternalModules :: (MonadIO m) => CabalConfiguration -> m [ExternModule]
getExternalModules _ = return []

-- | Parse a string as a cabal project configuration
parseCabalConfiguration :: String -> Either (ProjectError u) CabalConfiguration
parseCabalConfiguration s = case parsePackageDescription s of
    ParseFailed err -> Left $ InvalidOperation (show err) ""
    ParseOk _ x -> Right $ CabalConfiguration $ x

-- | Given a file path, determine if it refers to a directory or cabal file.
-- If it refers to a cabal file, get the directory it is in as well
-- If it refers to a directory, ensure that the X.cabal file is present in that directory
-- In either case, if successful, return the cabal project root directory and the path to the cabal file
getCabalDirectoryAndConfigPath :: (MonadIO m) => FilePath -> ProjectResult m u (FilePath,FilePath)
getCabalDirectoryAndConfigPath path = do
    let upperDirectory = takeDirectory path
        projectName = takeBaseName $ dropExtension path
        innerFile = path </> projectName ++ ".cabal"
        isCabalFile = ".cabal" `isSuffixOf` path
    isDirectory <- wrapIOError $ doesDirectoryExist path
    isFile <- wrapIOError $ doesFileExist path
    innerIsFile <- wrapIOError $ doesFileExist innerFile
    upperIsDirectory <- wrapIOError $ doesDirectoryExist upperDirectory
    if isDirectory && innerIsFile
        then return (path, innerFile)
        else if isFile && isCabalFile && upperIsDirectory
            then return (upperDirectory, path)
            else throwE $ InvalidOperation (path ++ " is not a valid cabal file or cabal project root") ""
{-
getCabalDirectory :: (MonadIO m) => FilePath -> ProjectResult m u FilePath
getCabalDirectory path = do
    p <- wrapIOError $ doesDirectoryExist path
    if p
        then do
            p <- wrapIOError $ doesFileExist $ path ++ ".cabal"
            if p
                then return $ path
                else throwE $ InvalidOperation (path ++ " is not a valid cabal file or cabal project root") ""
        else if ".cabal" `isSuffixOf` path
            then do
                p <- wrapIOError $ doesFileExist path
                then takeDirectoryName path
                else throwE $ InvalidOperation (path ++ " is not a valid cabal file or cabal project root") ""
            else InvalidOperation (path ++ " is not a valid cabal file or cabal project root") ""
            
            
            
getCabalConfigPath :: (MonadIO m) => FilePath -> ProjectResult m u FilePath
getCabalConfigPath path = do
    if ".cabal" `isSuffixOf` path
        then do
            p <- wrapIOError $ doesFileExist path
            if p
                then return path
                else do
                    let path' = take (length path - length ".cabal") path
                    p <- wrapIOError $ doesDirectoryExist path'
                    if p
                        then return $ path'
                        else throwE $ InvalidOperation (path ++ " is not a valid cabal file or cabal project root") ""
        else throwE $ InvalidOperation (path ++ " is not a valid cabal file or cabal project root") ""
-}

-- | Given either a cabal project root or a cabal project file, get the project root
getCabalDirectory :: (MonadIO m) => FilePath -> ProjectResult m u FilePath
getCabalDirectory = liftM fst . getCabalDirectoryAndConfigPath

-- | Given either a cabal project root or a cabal project file, get the cabal project file
getCabalConfigPath :: (MonadIO m) => FilePath -> ProjectResult m u FilePath
getCabalConfigPath = liftM snd . getCabalDirectoryAndConfigPath

-- | Not entirely sure what this is supposed to do
makeProjectInfo :: FilePath -> CabalConfiguration -> ProjectInfo
makeProjectInfo _ _ = ProjectInfo

-- | Take a project configuration and produce the string to write to file
reformCabalConfiguration :: CabalConfiguration -> String
reformCabalConfiguration (CabalConfiguration desc)
    = showGenericPackageDescription desc

-- | A project type
data ProjectType
    = LibraryProject Library -- ^ A library project
    | ExecutableProject Executable -- ^ An executable project

-- | Get the type of a project
getProjectType :: (Monad m) => ProjectResult (CabalProject m) u ProjectType
getProjectType = withOpenedProject $ \_ (CabalConfiguration desc) -> do
    case condLibrary desc of
        Just CondNode{condTreeData=lib} -> return $ LibraryProject $ lib
        Nothing -> case condExecutables desc of
            [(str,CondNode{condTreeData=exe})] -> return $ ExecutableProject $ exe
            _ -> throwE $ Unsupported "Cabal project must have 1 and only 1 source dir"

-- | Get the directory that a project stores its code files in
getProjectSourceRoot :: (Monad m) => ProjectResult (CabalProject m) u FilePath
getProjectSourceRoot = do
    ptype <- getProjectType
    case ptype of
        LibraryProject lib -> case hsSourceDirs $ libBuildInfo $ lib of
            [path] -> return path
            _ -> throwE $ Unsupported "Cabal project must have 1 and only 1 source dir"
        ExecutableProject exe -> case hsSourceDirs $ buildInfo $ exe of
            [path] -> return path
            _ -> throwE $ Unsupported "Cabal project must have 1 and only 1 source dir"

-- | Write a module out to file
writeModule :: (MonadIO m) => Module -> ProjectResult (CabalProject m) u ()
writeModule m = do -- withOpenedProject $ \_ (CabalConfiguration desc) -> do
    let info = Module.info m
    path <- case info of
        (ModuleInfo (Symbol s)) -> return $ ModuleName.toFilePath $ ModuleName.fromString s
        _ -> throwE $ InvalidOperation "Cannot add unnamed modules to a cabal probject" ""
    let text = Module.toFile m
    root <- getProjectSourceRoot
    let modulePath = root </> path ++ ".hs"
    let moduleDir = takeDirectory modulePath
    wrapIOError $ do
        createDirectoryIfMissing True moduleDir
        writeFile modulePath text

-- | Add a module to a project's list of modules
addModuleToConfig :: (ProjectStateM m) => ModuleInfo -> ProjectResult (CabalProject m) u ()
addModuleToConfig m = do
    moduleName <- case m of
        (ModuleInfo (Symbol name)) -> return $ ModuleName.fromString name
        _ -> throwE $ Unsupported "Cannot add unamed module to a cabal project"
    ptype <- getProjectType
    ptype' <- case ptype of
        LibraryProject lib -> do
            let lib' = lib
                     { exposedModules = exposedModules lib ++ [moduleName] }
            return $ LibraryProject lib'
        ExecutableProject exe -> do
            let exe' = exe
                     { buildInfo = (buildInfo exe)
                                 { otherModules = otherModules (buildInfo exe) ++ [moduleName]
                                 }
                     }
            return $ ExecutableProject exe'
    updateConfig ptype'

-- | Remove a module from a project's list of modules
removeModuleFromConfig :: (ProjectStateM m) => ModuleInfo -> ProjectResult (CabalProject m) u ()
removeModuleFromConfig m = do
    moduleName <- case m of
        (ModuleInfo (Symbol name)) -> return $ ModuleName.fromString name
        _ -> throwE $ Unsupported "Cannot add unamed module to a cabal project"
    ptype <- getProjectType
    ptype' <- case ptype of
        LibraryProject lib -> do
            let lib' = lib
                     { exposedModules = delete moduleName $ exposedModules lib }
            return $ LibraryProject lib'
        ExecutableProject exe -> do
            let exe' = exe
                     { buildInfo = (buildInfo exe)
                                 { otherModules = delete moduleName $ otherModules (buildInfo exe)
                                 }
                     }
            return $ ExecutableProject exe'
    updateConfig ptype'
{- do
    fsp <- lift getFsp
    case fsp of
        Opened (Just (CabalProjectInfo path cabalConfig)) -> do
            let cabalConfig' = removeModuleFromConfig' m cabalConfig
            lift $ putFsp $ Opened $ Just $ CabalProjectInfo path cabalConfig
        _ -> throwE $ InvalidOperation "No project to remove from" ""
-}


instance (MonadIO m, ProjectStateM m) => ProjectM (CabalProject m) where
    -- | Set up an empty project
    new i = do
        lift $ putFsp $ Opened (Nothing)
        lift $ putProject $ Project.new i
    -- | Open a project by reading the .cabal file and using it to find all of its modules
    load = do
        fsp <- lift getFsp
        case fsp of
            ToOpen path -> do
--                cabalDirectory <- getCabalDirectory path
--                cabalConfigPath <- getCabalConfigPath cabalDirectory
                absPath <- wrapIOError $ makeAbsolute path
                (cabalDirectory,cabalConfigPath) <- getCabalDirectoryAndConfigPath absPath
                wrapIOError $ setCurrentDirectory cabalDirectory
                cabalConfig <- wrapReadFile cabalConfigPath
                                    >>= ExceptT . return . parseCabalConfiguration
                lift $ putFsp $ Opened $ Just $ CabalProjectInfo cabalDirectory cabalConfig
                internalModules <- getInternalModules
                externalModules <- getExternalModules cabalConfig
                lift $ putProject $ Project.new $ makeProjectInfo cabalDirectory cabalConfig
                forM_ internalModules $  \m -> modifyProjectE $ \p -> Project.addModule p m
                mapM_ addExternModule externalModules
            Unopened -> throwE $ InvalidOperation "No path specified for opening" ""
            Opened Nothing -> throwE $ InvalidOperation "Cannot re-open a digested project" ""
            Opened (Just (CabalProjectInfo path _)) -> do
                lift $ putFsp $ ToOpen path
                load
    -- | Write out the .cabal file and each of its modules
    finalize = do
        fsp <- lift getFsp
        case fsp of
            Opened (Just (CabalProjectInfo path cabalConfig)) -> do
                let cabalConfigOutput = reformCabalConfiguration cabalConfig 
                cabalConfigPath <- getCabalConfigPath path 
                wrapIOError $ writeFile cabalConfigPath cabalConfigOutput
                moduleInfos <- getModules
                modules <- mapM getModule moduleInfos
                mapM_ writeModule modules 
            _ -> throwE $ InvalidOperation "No project to save" ""
    
    -- | TODO
    editProjectInfo _ = throwE $ InvalidOperation "TODO" "editProjectInfo"

    -- | Add a module to the project as well as the cabal configuration
    addModule m = do
        modifyProjectE $ \p -> Project.addModule p m
        addModuleToConfig $ Module.info m

    addExternModule m = do
        modifyProjectE $ \p -> Project.addExternModule p m
    createModule i = do
        modifyProjectE $ \p -> Project.createModule p i
        addModuleToConfig i
    getModule i = do
        ExceptT $ getsProject $ \p -> Project.getModule p i
    getExternModule i = do
        ExceptT $ getsProject $ \p -> Project.getExternModule p i
    getModules = do
        lift $ getsProject Project.allModules
    editModule i f = do
        modifyProjectE $ \p -> Project.editModule p i f
    removeModule i = do
        modifyProjectE $ \p -> Project.removeModule p i
        removeModuleFromConfig i

    addDeclaration i d = do
        modifyProjectE $ \p -> Project.addDeclaration p i d
    getDeclaration i di = do
        ExceptT $ getsProject $ \p -> getChild <$> Project.getDeclaration p (ModuleChild i di)
    getDeclarations i = do
        ExceptT $ getsProject $ \p -> map getChild <$> Project.allDeclarationsIn p i
    editDeclaration i di f = do
        modifyProjectE $ \p -> Project.editDeclaration p (ModuleChild i di) f
    removeDeclaration i di = do
        modifyProjectE $ \p -> Project.removeDeclaration p (ModuleChild i di)

    addImport mi i = do
        modifyProjectER $ \p -> Project.addImport p mi i
    getImport mi iid = do
        ExceptT $ getsProject $ \p -> Project.getImport p mi iid
    removeImport mi i = do
        modifyProjectE $ \p -> Project.removeImport p mi i
    getImports mi = do
        ExceptT $ getsProject $ \p -> Project.getImports p mi

    addExport mi e = do
        modifyProjectER $ \p -> Project.addExport p mi e
    getExport mi eid = do
        ExceptT $ getsProject $ \p -> Project.getExport p mi eid
    removeExport mi e = do
        modifyProjectE $ \p -> Project.removeExport p mi e
    exportAll mi = do
        modifyProjectE $ \p -> Project.exportAll p mi
    exportNothing mi = do
        modifyProjectE $ \p -> Project.exportNothing p mi
    getExports mi = do
        ExceptT $ getsProject $ \p -> Project.getExports p mi



instance (MonadIO m, ProjectStateM m) => ViewerMonad (CabalProject m) where
    -- | Set the Read file to be opened
    setFileToOpen path = lift $ putFsp $ ToOpen path
    -- | Set the path to be digested
    setDirectoryToOpen path = lift $ putFsp $ ToOpen path
    -- | Set the path to Show to
    setTargetPath path = throwE $ Unsupported $ "TODO"
    -- | Check if either there is a new project, digested path, or Read'd file
    hasOpenedProject = do
        fsp <- getFsp
        case fsp of
            Opened _ -> return True
            _ -> return False
    createNewFile path = do
        lift $ lift $ putProject $ Project.new ProjectInfo
        lift $ putFsp $ Opened Nothing
        setTargetPath path
    createNewDirectory path = do
        lift $ lift $ putProject $ Project.new ProjectInfo
        lift $ putFsp $ Opened Nothing
        setTargetPath path
    prepareBuild = finalize


instance PseudoStateT CabalProject FileSystemProject where
    runPseudoStateT = runStateT . runCabalProjectInternal
