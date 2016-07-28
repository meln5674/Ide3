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
in a solution, and stores declarations in the module files as normal
-}
module CabalFilesystemProject
    ( CabalSolution(CabalSolution)
    , FileSystemSolution (Unopened)
    , runCabalSolution
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
import Distribution.Package
import Distribution.PackageDescription
import Distribution.PackageDescription.Parse
import Distribution.PackageDescription.Configuration
import Distribution.PackageDescription.PrettyPrint

import Ide3.Types
import Ide3.Utils
import Ide3.Monad
import Ide3.Mechanism.State hiding (new, load, finalize)

import qualified Ide3.Env.Solution as Solution
import qualified Ide3.Env.Project as Project
import qualified Ide3.Module as Module

import Ide3.Env

import ViewerMonad
import PseudoState

-- | State of the mechanism
data FileSystemSolution
    -- | A file containing a dump of a Solution is to be opened
    = ToOpen FilePath
    -- | No solution opened
    | Unopened
    -- | A digested path or dump file is opened if Just, a new solution if Nothing
    | Opened (Maybe CabalSolutionInfo)

-- | Path to a cabal directory and the configuration
data CabalSolutionInfo = CabalSolutionInfo FilePath CabalConfiguration

-- | Wrapper around the cabal solution configuration
newtype CabalConfiguration = CabalConfiguration GenericPackageDescription

-- | Type wrapper for using a cabal file as a solution setup and the module
-- files to store code normally
newtype CabalSolution m a
    = CabalSolution { runCabalSolutionInternal :: StateT FileSystemSolution m a }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadTrans
    , MonadIO
    , SolutionStateM
    )

-- | Run a cabal solution
runCabalSolution :: CabalSolution m a -> FileSystemSolution -> m (a, FileSystemSolution)
runCabalSolution = runStateT . runCabalSolutionInternal

deriving instance (MonadMask m) => MonadMask (CabalSolution m)
deriving instance (MonadCatch m) => MonadCatch (CabalSolution m)
deriving instance (MonadThrow m) => MonadThrow (CabalSolution m)

-- | Get the state of the solution
getFsp :: (Monad m) => CabalSolution m FileSystemSolution
getFsp = CabalSolution get

-- | Set the state of the solution
putFsp :: (Monad m) => FileSystemSolution -> CabalSolution m ()
putFsp = CabalSolution . put 

-- | Perform an action on an open solution, or throw an error if not open
withOpenedSolution :: (Monad m) 
                  => (FilePath -> CabalConfiguration -> SolutionResult (CabalSolution m) u a)
                  -> SolutionResult (CabalSolution m) u a
withOpenedSolution f = do
    fsp <- lift getFsp
    case fsp of
        Opened (Just (CabalSolutionInfo path config)) -> f path config
        _ -> throwE $ InvalidOperation "No open solution" ""

-- | Update a solution's library
updateLibrary :: ProjectInfo -> Library -> GenericPackageDescription -> GenericPackageDescription
updateLibrary pi lib desc@GenericPackageDescription{condLibrary=Just condLib}
--    = desc{condLibrary=Just condLib{condTreeData = lib}} -- TODO: Update library
    = undefined
    
-- Update a solution's executable
updateExecutable :: ProjectInfo -> Executable -> GenericPackageDescription -> GenericPackageDescription
updateExecutable pi exe desc@GenericPackageDescription{condExecutables=[(str,condExe)]}
--    = desc{condExecutables=[(str,condExe{condTreeData=exe})]} -- TODO: Update execuatable
    = undefined
    
-- | Update the solution configuration
updateConfig :: (Monad m) 
             => ProjectInfo
             -> ProjectType 
             -> SolutionResult (CabalSolution m) u ()
updateConfig pi (LibraryProject lib) = withOpenedSolution $ \path (CabalConfiguration desc) -> do
    let desc' = updateLibrary pi lib desc
    lift $ putFsp $ Opened $ Just $ CabalSolutionInfo path $ CabalConfiguration desc'
updateConfig pi (ExecutableProject exe) = withOpenedSolution $ \path (CabalConfiguration desc) -> do
    let desc' = updateExecutable pi exe desc
    lift $ putFsp $ Opened $ Just $ CabalSolutionInfo path $ CabalConfiguration desc'

libraryInfo :: ProjectInfo 
libraryInfo = ProjectInfo "LIBRARY"

isProjectInfo :: ProjectInfo -> ProjectType -> Bool
isProjectInfo pi (LibraryProject _) = pi == libraryInfo
isProjectInfo (ProjectInfo projectName) (ExecutableProject exe) = exeName exe == projectName

getProject :: Monad m => ProjectInfo -> SolutionResult (CabalSolution m) u ProjectType
getProject pi = do
    ps <- getAllProjects
    case filter (isProjectInfo pi) ps of
        [] -> throwE $ ProjectNotFound pi ""
        [p] -> return p
        _ -> throwE $ DuplicateProject pi ""

-- | Get a list of modules from a cabal solution
getInternalModules :: (MonadIO m) => ProjectInfo -> SolutionResult (CabalSolution m) u [Module]
getInternalModules pi = do
    ptype <- getProject pi
    root <- getProjectSourceRoot ptype
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
    
    
-- | Get the external modules used by a cabal solution. * INCOMPLETE *
getExternalModules :: (MonadIO m) => ProjectInfo -> SolutionResult (CabalSolution m) u [ExternModule]
getExternalModules pi = return []
{-
getExternalModules = do
    ptype <- getProjectType
    let binfo = case ptype of
            LibraryProject lib -> libBuildInfo lib
            ExecutableProject exe -> buildInfo exe
        depends = buildDepends binfo
-}    
-- | Parse a string as a cabal solution configuration
parseCabalConfiguration :: String -> Either (SolutionError u) CabalConfiguration
parseCabalConfiguration s = case parsePackageDescription s of
    ParseFailed err -> Left $ InvalidOperation (show err) ""
    ParseOk _ x -> Right $ CabalConfiguration $ x

-- | Given a file path, determine if it refers to a directory or cabal file.
-- If it refers to a cabal file, get the directory it is in as well
-- If it refers to a directory, ensure that the X.cabal file is present in that directory
-- In either case, if successful, return the cabal solution root directory and the path to the cabal file
getCabalDirectoryAndConfigPath :: (MonadIO m) => FilePath -> SolutionResult m u (FilePath,FilePath)
getCabalDirectoryAndConfigPath path = do
    let upperDirectory = takeDirectory path
        solutionName = takeBaseName $ dropExtension path
        innerFile = path </> solutionName ++ ".cabal"
        isCabalFile = ".cabal" `isSuffixOf` path
    isDirectory <- wrapIOError $ doesDirectoryExist path
    isFile <- wrapIOError $ doesFileExist path
    innerIsFile <- wrapIOError $ doesFileExist innerFile
    upperIsDirectory <- wrapIOError $ doesDirectoryExist upperDirectory
    if isDirectory && innerIsFile
        then return (path, innerFile)
        else if isFile && isCabalFile && upperIsDirectory
            then return (upperDirectory, path)
            else throwE $ InvalidOperation (path ++ " is not a valid cabal file or cabal solution root") ""
{-
getCabalDirectory :: (MonadIO m) => FilePath -> SolutionResult m u FilePath
getCabalDirectory path = do
    p <- wrapIOError $ doesDirectoryExist path
    if p
        then do
            p <- wrapIOError $ doesFileExist $ path ++ ".cabal"
            if p
                then return $ path
                else throwE $ InvalidOperation (path ++ " is not a valid cabal file or cabal solution root") ""
        else if ".cabal" `isSuffixOf` path
            then do
                p <- wrapIOError $ doesFileExist path
                then takeDirectoryName path
                else throwE $ InvalidOperation (path ++ " is not a valid cabal file or cabal solution root") ""
            else InvalidOperation (path ++ " is not a valid cabal file or cabal solution root") ""
            
            
            
getCabalConfigPath :: (MonadIO m) => FilePath -> SolutionResult m u FilePath
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
                        else throwE $ InvalidOperation (path ++ " is not a valid cabal file or cabal solution root") ""
        else throwE $ InvalidOperation (path ++ " is not a valid cabal file or cabal solution root") ""
-}

-- | Given either a cabal solution root or a cabal solution file, get the solution root
getCabalDirectory :: (MonadIO m) => FilePath -> SolutionResult m u FilePath
getCabalDirectory = liftM fst . getCabalDirectoryAndConfigPath

-- | Given either a cabal solution root or a cabal solution file, get the cabal solution file
getCabalConfigPath :: (MonadIO m) => FilePath -> SolutionResult m u FilePath
getCabalConfigPath = liftM snd . getCabalDirectoryAndConfigPath

-- | Not entirely sure what this is supposed to do
makeSolutionInfo :: FilePath -> CabalConfiguration -> SolutionInfo
makeSolutionInfo _ (CabalConfiguration d) 
    = SolutionInfo 
    $ unPackageName 
    $ pkgName 
    $ package 
    $ packageDescription 
    $ d

-- | Take a solution configuration and produce the string to write to file
reformCabalConfiguration :: CabalConfiguration -> String
reformCabalConfiguration (CabalConfiguration desc)
    = showGenericPackageDescription desc

-- | A solution type
data ProjectType
    = LibraryProject Library -- ^ A library solution
    | ExecutableProject Executable -- ^ An executable solution


getAllProjects :: (Monad m) => SolutionResult (CabalSolution m) u [ProjectType]
getAllProjects = withOpenedSolution $ \_ (CabalConfiguration desc) -> do
    let exes = map (ExecutableProject . condTreeData . snd) $ condExecutables desc
    case condTreeData <$> condLibrary desc of
        Just lib -> return $ LibraryProject lib : exes
        Nothing -> return exes

getProjectInfo :: ProjectType -> ProjectInfo
getProjectInfo (LibraryProject _) = libraryInfo
getProjectInfo (ExecutableProject exe) = ProjectInfo $ exeName $ exe

loadProject :: (MonadIO m) => ProjectInfo -> SolutionResult (CabalSolution m) u Project
loadProject pi = do
    modules <- getInternalModules pi
    externModules <- getExternalModules pi
    let p = Project.new pi
    p' <- mapDescent2_ Project.addModule p modules
    mapDescent2_ Project.addExternModule p' externModules
    

-- | Get the type of a solution
getProjectType :: (Monad m) => SolutionResult (CabalSolution m) u ProjectType
getProjectType = withOpenedSolution $ \_ (CabalConfiguration desc) -> do
    case condLibrary desc of
        Just CondNode{condTreeData=lib} -> return $ LibraryProject $ lib
        Nothing -> case condExecutables desc of
            [(str,CondNode{condTreeData=exe})] -> return $ ExecutableProject $ exe
            _ -> throwE $ Unsupported "Cabal solution must have 1 and only 1 source dir"

-- | Get the directory that a solution stores its code files in
getProjectSourceRoot :: (Monad m) => ProjectType -> SolutionResult (CabalSolution m) u FilePath
getProjectSourceRoot ptype = do
    case ptype of
        LibraryProject lib -> case hsSourceDirs $ libBuildInfo $ lib of
            [path] -> return path
            _ -> throwE $ Unsupported "Cabal solution must have 1 and only 1 source dir"
        ExecutableProject exe -> case hsSourceDirs $ buildInfo $ exe of
            [path] -> return path
            _ -> throwE $ Unsupported "Cabal solution must have 1 and only 1 source dir"

-- | Write a module out to file
writeModule :: (MonadIO m) => ProjectInfo -> Module -> SolutionResult (CabalSolution m) u ()
writeModule pi m = do -- withOpenedSolution $ \_ (CabalConfiguration desc) -> do
    let info = Module.info m
    path <- case info of
        (ModuleInfo (Symbol s)) -> return $ ModuleName.toFilePath $ ModuleName.fromString s
        _ -> throwE $ InvalidOperation "Cannot add unnamed modules to a cabal probject" ""
    let text = Module.toFile m
    ptype <- getProject pi
    root <- getProjectSourceRoot ptype
    let modulePath = root </> path ++ ".hs"
    let moduleDir = takeDirectory modulePath
    wrapIOError $ do
        createDirectoryIfMissing True moduleDir
        writeFile modulePath text

-- | Add a module to a solution's list of modules
addModuleToConfig :: (SolutionStateM m) 
                  => ProjectInfo 
                  -> ModuleInfo 
                  -> SolutionResult (CabalSolution m) u ()
addModuleToConfig pi mi = do
    moduleName <- case mi of
        (ModuleInfo (Symbol name)) -> return $ ModuleName.fromString name
        _ -> throwE $ Unsupported "Cannot add unamed module to a cabal solution"
    ptype <- getProject pi
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
    updateConfig pi ptype'

-- | Remove a module from a solution's list of modules
removeModuleFromConfig :: (SolutionStateM m) 
                       => ProjectInfo 
                       -> ModuleInfo 
                       -> SolutionResult (CabalSolution m) u ()
removeModuleFromConfig pi mi = do
    moduleName <- case mi of
        (ModuleInfo (Symbol name)) -> return $ ModuleName.fromString name
        _ -> throwE $ Unsupported "Cannot add unamed module to a cabal solution"
    ptype <- getProject pi
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
    updateConfig pi ptype'
{- do
    fsp <- lift getFsp
    case fsp of
        Opened (Just (CabalSolutionInfo path cabalConfig)) -> do
            let cabalConfig' = removeModuleFromConfig' m cabalConfig
            lift $ putFsp $ Opened $ Just $ CabalSolutionInfo path cabalConfig
        _ -> throwE $ InvalidOperation "No solution to remove from" ""
-}


instance (MonadIO m, SolutionStateM m) => SolutionM (CabalSolution m) where
    -- | Set up an empty solution
    new i = do
        lift $ putFsp $ Opened (Nothing)
        lift $ putSolution $ Solution.new i
    -- | Open a solution by reading the .cabal file and using it to find all of its modules
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
                lift $ putFsp $ Opened $ Just $ CabalSolutionInfo cabalDirectory cabalConfig
                projects <- getAllProjects >>= mapM (loadProject . getProjectInfo)
                {-
                lift $ putSolution $ Solution.new $ makeSolutionInfo cabalDirectory cabalConfig
                forM_ internalModules $  \m -> modifySolutionEnv $ \p -> Solution.addModule p m
                mapM_ addExternModule externalModules
                -}
                undefined
            Unopened -> throwE $ InvalidOperation "No path specified for opening" ""
            Opened Nothing -> throwE $ InvalidOperation "Cannot re-open a digested solution" ""
            Opened (Just (CabalSolutionInfo path _)) -> do
                lift $ putFsp $ ToOpen path
                load
    -- | Write out the .cabal file and each of its modules
    finalize = do
        fsp <- lift getFsp
        case fsp of
            Opened (Just (CabalSolutionInfo path cabalConfig)) -> do
                let cabalConfigOutput = reformCabalConfiguration cabalConfig 
                cabalConfigPath <- getCabalConfigPath path 
                wrapIOError $ writeFile cabalConfigPath cabalConfigOutput
                let writeProject ptype = do
                        let pi = getProjectInfo ptype
                        moduleInfos <- getModules pi
                        modules <- mapM (getModule pi) moduleInfos
                        mapM_ (writeModule pi) modules
                getAllProjects >>= mapM_ writeProject
            _ -> throwE $ InvalidOperation "No solution to save" ""


    -- | TODO
    editSolutionInfo _ = throwE $ InvalidOperation "TODO" "editSolutionInfo"
--    editSolutionInfo f = lift $ modifySolution $ \s -> s{ solutionInfo = f $ solutionInfo s }
    
    addProject a = modifySolutionEnv $ \s -> runDescent2 Solution.addProject s a
    removeProject a = modifySolutionEnv $ \s -> runDescent2 Solution.removeProject s a
    getProjects = modifySolutionEnv $ \s -> runDescent1 Solution.getProjects s
    editProjectInfo a b = modifySolutionEnv $ \s -> runDescent3 Solution.editProjectInfo s a b

    addModule a b = do
        modifySolutionEnv $ \s -> runDescent3 Solution.addModule s a b
        addModuleToConfig a $ Module.info b

    addExternModule a b = modifySolutionEnv $ \s -> runDescent3 Solution.addExternModule s a b
    createModule a b = do
        modifySolutionEnv $ \s -> runDescent3 Solution.createModule s a b
        addModuleToConfig a b

    getModule a b = modifySolutionEnv $ \s -> runDescent3 Solution.getModule s a b
    getExternModule a b = modifySolutionEnv $ \s -> runDescent3 Solution.getExternModule s a b
    getModules a = modifySolutionEnv $ \s -> runDescent2 Solution.allModules s a
    editModule a b c = modifySolutionEnv $ \s -> runDescent4 Solution.editModule s a b c
    removeModule a b = do
        modifySolutionEnv $ \s -> runDescent3 Solution.removeModule s a b
        removeModuleFromConfig a b


    addDeclaration a b c = modifySolutionEnv $ \s -> runDescent4 Solution.addDeclaration s a b c
    getDeclaration a b c  = modifySolutionEnv $ \s -> runDescent4 Solution.getDeclaration s a b c
    getDeclarations a b = modifySolutionEnv $ \s -> runDescent3 Solution.getDeclarations s a b
    editDeclaration a b c d = modifySolutionEnv $ \s -> runDescent5 Solution.editDeclaration s a b c d
    removeDeclaration a b c = modifySolutionEnv $ \s -> runDescent4 Solution.removeDeclaration s a b c

    addImport a b c = modifySolutionEnv $ \s -> runDescent4 Solution.addImport s a b c
    getImport a b c = modifySolutionEnv $ \s -> runDescent4 Solution.getImport s a b c
    removeImport a b c = modifySolutionEnv $ \s -> runDescent4 Solution.removeImport s a b c
    getImports a b = modifySolutionEnv $ \s -> runDescent3 Solution.getImports s a b
    
    addExport a b c = modifySolutionEnv $ \s -> runDescent4 Solution.addExport s a b c
    getExport a b c = modifySolutionEnv $ \s -> runDescent4 Solution.getExport s a b c
    removeExport a b c = modifySolutionEnv $ \s -> runDescent4 Solution.removeExport s a b c
    exportAll a b = modifySolutionEnv $ \s -> runDescent3 Solution.exportAll s a b
    exportNothing a b = modifySolutionEnv $ \s -> runDescent3 Solution.exportNothing s a b
    getExports a b = modifySolutionEnv $ \s -> runDescent3 Solution.getExports s a b

    addPragma a b c = modifySolutionEnv $ \s -> runDescent4 Solution.addPragma s a b c
    removePragma a b c = modifySolutionEnv $ \s -> runDescent4 Solution.removePragma s a b c
    getPragmas a b = modifySolutionEnv $ \s -> runDescent3 Solution.getPragmas s a b
    

instance (MonadIO m, SolutionStateM m) => ViewerMonad (CabalSolution m) where
    -- | Set the Read file to be opened
    setFileToOpen path = lift $ putFsp $ ToOpen path
    -- | Set the path to be digested
    setDirectoryToOpen path = lift $ putFsp $ ToOpen path
    -- | Set the path to Show to
    setTargetPath path = throwE $ Unsupported $ "TODO"
    -- | Check if either there is a new solution, digested path, or Read'd file
    hasOpenedSolution = do
        fsp <- getFsp
        case fsp of
            Opened _ -> return True
            _ -> return False
    createNewFile path = do
        lift $ lift $ putSolution $ Solution.new $ SolutionInfo $ takeBaseName path
        lift $ putFsp $ Opened Nothing
        setTargetPath path
    createNewDirectory path = do
        lift $ lift $ putSolution $ Solution.new $ SolutionInfo $ takeBaseName path
        lift $ putFsp $ Opened Nothing
        setTargetPath path
    prepareBuild = finalize


instance PseudoStateT CabalSolution FileSystemSolution where
    runPseudoStateT = runStateT . runCabalSolutionInternal
