{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE LambdaCase #-}
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
{-# LANGUAGE FlexibleInstances #-}
module CabalFilesystemSolution
    ( CabalSolution(CabalSolution)
    , FileSystemSolution (Unopened)
    , runCabalSolution
    ) where

import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Monoid

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
import Ide3.NewMonad

import qualified Ide3.Env.Solution as Solution
import qualified Ide3.Project as Project
import qualified Ide3.Env.Project as Project
import qualified Ide3.Module as Module

import Ide3.Env

import ViewerMonad
import PseudoState
import CabalMonad

-- | State of the mechanism
data FileSystemSolution
    -- | A file containing a dump of a Solution is to be opened
    = ToOpen FilePath
    -- | No solution opened
    | Unopened
    -- | A digested path or dump file is opened if Just, a new solution if Nothing
    | Opened (Maybe CabalSolutionInfo)

-- | Path to a cabal directory and the configuration
data CabalSolutionInfo = CabalSolutionInfo
    { solutionPath :: FilePath
    , cabalConfig :: CabalConfiguration
    , modulePathMap :: ModulePathMap
    }

-- | Map from modules to file paths, used to keep track of which modules came
-- from which source directory
type ModulePathMap = Map (ProjectInfo,ModuleInfo) FilePath

-- | Wrapper around the cabal solution configuration
newtype CabalConfiguration = CabalConfiguration GenericPackageDescription 

addModulePath :: ProjectInfo 
              -> ModuleInfo 
              -> FilePath 
              -> CabalSolutionInfo
              -> CabalSolutionInfo
addModulePath pi mi p conf = conf { modulePathMap = Map.insert (pi,mi) p $ modulePathMap conf }

-- | Type wrapper for using a cabal file as a solution setup and the module
-- files to store code normally
newtype CabalSolution m a
    = CabalSolution { runCabalSolutionInternal :: StateT FileSystemSolution m a }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    )

instance MonadBounce CabalSolution where
    bounce = ExceptT . CabalSolution . lift . runExceptT

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

modifyFsp :: (Monad m) 
          => (FileSystemSolution -> FileSystemSolution) 
          -> CabalSolution m ()
modifyFsp = CabalSolution . modify

-- | Perform an action on an open solution, or throw an error if not open
withOpenedSolution :: (Monad m) 
                  => (CabalSolutionInfo -> SolutionResult (CabalSolution m) u a)
                  -> SolutionResult (CabalSolution m) u a
withOpenedSolution f = do
    fsp <- lift getFsp
    case fsp of
        Opened (Just si) -> f si
        _ -> throwE $ InvalidOperation "No open solution" ""
{-
-- | Update a solution's library
updateLibrary :: ProjectInfo -> Library -> GenericPackageDescription -> GenericPackageDescription
updateLibrary pi lib desc@GenericPackageDescription{condLibrary=Just condLib}
    = desc{condLibrary=Just condLib{condTreeData = lib}}
updateLibrary pi lib desc
   = desc{condLibrary=Just CondNode
                     { condTreeData = lib
                     , condTreeComponents = []
                     , condTreeConstraints = []
                     }
         }
    
-- Update a solution's executable
updateExecutable :: ProjectInfo -> Executable -> GenericPackageDescription -> GenericPackageDescription
updateExecutable (ProjectInfo n) exe desc@GenericPackageDescription{condExecutables=exeList}
    = desc{condExecutables=exeList'}
  where
    exeMap = Map.fromList exeList
    exeMap' = case Map.lookup n exeMap of
        Nothing -> Map.insert n CondNode
                                { condTreeData = exe
                                , condTreeConstraints = []
                                , condTreeComponents = []
                                }
                                exeMap
        Just tree -> Map.insert n tree{condTreeData=exe} exeMap
    exeList' = Map.toList exeMap'
-}      
{-
-- | Update the solution configuration
updateConfig :: (Monad m) 
             => ProjectInfo
             -> CabalProject
             -> SolutionResult (CabalSolution m) u ()
updateConfig pi (LibraryProject lib) = withOpenedSolution $ \path (CabalConfiguration desc) -> do
    let desc' = updateLibrary pi lib desc
    lift $ putFsp $ Opened $ Just $ CabalSolutionInfo path $ CabalConfiguration desc'
updateConfig pi (ExecutableProject exe) = withOpenedSolution $ \path (CabalConfiguration desc) -> do
    let desc' = updateExecutable pi exe desc
    lift $ putFsp $ Opened $ Just $ CabalSolutionInfo path $ CabalConfiguration desc'
-}

libraryInfo :: ProjectInfo 
libraryInfo = ProjectInfo "library"

{-
isProjectInfo :: ProjectInfo -> ProjectType -> Bool
isProjectInfo pi (LibraryProject _) = pi == libraryInfo
isProjectInfo (ProjectInfo projectName) (ExecutableProject exe) = exeName exe == projectName


getProject :: Monad m => ProjectInfo -> SolutionResult (CabalSolution m) u CabalProject
getProject pi = do
    ps <- getAllProjects
    case filter (isProjectInfo pi) ps of
        [] -> throwE $ ProjectNotFound pi ""
        [p] -> return p
        _ -> throwE $ DuplicateProject pi ""
-}

toModuleName :: ModuleInfo -> ModuleName.ModuleName
toModuleName (ModuleInfo (Symbol s)) = ModuleName.fromString s

toModuleInfo :: ModuleName.ModuleName -> ModuleInfo
toModuleInfo mn = ModuleInfo $ Symbol $ intercalate "." $ ModuleName.components mn

locateModule :: (MonadIO m) => [FilePath] -> ModuleName.ModuleName -> m (Maybe FilePath)
locateModule paths mn = do
    let possiblePaths = map (</> (ModuleName.toFilePath mn ++ ".hs")) paths
    hits <- forM possiblePaths $ \path -> do
        exists <- liftIO $ doesFileExist path
        if exists
            then return $ Just path
            else return Nothing
    return $ getFirst $ mconcat $ map First hits

-- | Get a list of modules from a cabal solution
getInternalModules :: (MonadIO m) => ProjectInfo -> SolutionResult (CabalSolution m) u [(Module,FilePath)]
getInternalModules pi = do
    p <- lookupCabalProject pi
    roots <- getProjectSourceRoots p
    let modules = case p of
            LibraryProject lib -> exposedModules lib ++ (otherModules $ libBuildInfo lib)
            _ -> withBuildInfo p otherModules
    let mainModuleName = case p of
            LibraryProject lib -> Nothing
            ExecutableProject _ exe -> Just $ ModuleName.fromString $ takeBaseName $ modulePath exe
            TestSuiteProject _ test -> case testInterface test of
                TestSuiteExeV10 _ path -> Just $ ModuleName.fromString $ takeBaseName path
                TestSuiteLibV09 _ name -> Just $ name
                _ -> Nothing
            BenchmarkProject _ bench -> case benchmarkInterface bench of
                BenchmarkExeV10 _ path -> Just $ ModuleName.fromString $ takeBaseName path
                _ -> Nothing
    modulePaths <- forM modules $ \mn -> do
        maybePath <- locateModule roots mn
        return (mn, maybePath)
    mainModulePath <- case mainModuleName of
        Nothing -> return Nothing
        Just mn -> do
            path <- locateModule roots mn
            case path of
                Just path -> return $ Just path
                Nothing -> throwE $ InvalidOperation ("Can't find module " ++ show mn) ""
    moduleContents <- forM modulePaths $ \(mn,path) -> case path of
        Nothing -> throwE $ InvalidOperation ("Can't find module " ++ show mn) ""
        Just path -> do
            contents <- wrapReadFile path
            return (path,contents)
    moduleParses <- forM moduleContents $ \(path,contents) -> do
        m <- ExceptT $ return $ Module.parse contents $ Just path
        return (m,path)
    case mainModulePath of
        Just x -> do
            mainModuleContents <- wrapReadFile x
            (mainModule,_,_) <- ExceptT $ return $ Module.parseMain mainModuleContents $ Just x
            return $ (mainModule, x) : map (\((x,_,_),p) -> (x,p)) moduleParses
        Nothing -> return $ map (\((x,_,_),p) -> (x,p)) moduleParses
    
    
-- | Get the external modules used by a cabal solution. * INCOMPLETE *
getExternalModules :: (MonadIO m) 
                   => ProjectInfo 
                   -> SolutionResult (CabalSolution m) u [ExternModule]
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


{-
getAllProjects :: (Monad m) => SolutionResult (CabalSolution m) u [ProjectType]
getAllProjects = withOpenedSolution $ \_ (CabalConfiguration desc) -> do
    let exes = map (ExecutableProject . condTreeData . snd) $ condExecutables desc
    case condTreeData <$> condLibrary desc of
        Just lib -> return $ LibraryProject lib : exes
        Nothing -> return exes
-}

getProjectInfo :: CabalProject -> ProjectInfo
getProjectInfo (LibraryProject _) = libraryInfo
getProjectInfo (ExecutableProject i _) = i
getProjectInfo (TestSuiteProject i _) = i
getProjectInfo (BenchmarkProject i _) = i

loadProject :: ( MonadIO m
               , SolutionClass m
               , ProjectModuleClass m
               , ProjectExternModuleClass m
               ) 
            => ProjectInfo 
            -> SolutionResult (CabalSolution m) u ()
loadProject pi = do
    modules <- getInternalModules pi
    externModules <- getExternalModules pi
    addProject pi
    let addPaths = foldr (.) id 
                 $ flip map modules 
                 $ \(m,path) -> addModulePath pi (Module.info m) path
    (Opened (Just info)) <- lift getFsp
    let info' = addPaths info
    lift $ putFsp $ Opened $ Just info'
    forM_ modules $ bounce . addModule pi . fst -- This bounce is important, 
                                                -- as it targets the
                                                -- underlying monad, instead 
                                                -- of (CabalSolution m),
                                                -- preventing it from adding
                                                -- the module to the config
    forM_ externModules $ addExternModule pi
    
    --let p = Project.new pi
    --p' <- mapDescent2_ Project.addModule p modules
    --mapDescent2_ Project.addExternModule p' externModules
    

{-
-- | Get the type of a solution
getProjectType :: (Monad m) => SolutionResult (CabalSolution m) u ProjectType
getProjectType = withOpenedSolution $ \_ (CabalConfiguration desc) -> do
    case condLibrary desc of
        Just CondNode{condTreeData=lib} -> return $ LibraryProject $ lib
        Nothing -> case condExecutables desc of
            [(str,CondNode{condTreeData=exe})] -> return $ ExecutableProject $ exe
            _ -> throwE $ Unsupported "Cabal solution must have 1 and only 1 source dir"
-}

-- | Get the directory that a solution stores its code files in
getProjectSourceRoots :: (Monad m) => CabalProject -> SolutionResult (CabalSolution m) u [FilePath]
getProjectSourceRoots p = withBuildInfo p $ \b -> case hsSourceDirs b of
    [] -> return [""]
    x -> return x

-- | Write a module out to file
writeModule :: (MonadIO m) => ProjectInfo -> Module -> SolutionResult (CabalSolution m) u ()
writeModule pi m = withOpenedSolution $ \info -> do
    let mi = Module.info m
        modulePathLookup = Map.lookup (pi, mi) $ modulePathMap info
    modulePath <- case modulePathLookup of
        Just path -> return path
        Nothing -> throwE $ InternalError ("Can't find source directory for module " ++ show mi) ""
    let text = Module.toFile m
    ptype <- lookupCabalProject pi
    
    let moduleDir = takeDirectory modulePath
    wrapIOError $ do
        putStrLn moduleDir
        putStrLn modulePath
        createDirectoryIfMissing True moduleDir
        writeFile modulePath text

-- | Add a module to a solution's list of modules
addModuleToConfig :: (Monad m) 
                  => ProjectInfo 
                  -> ModuleInfo 
                  -> SolutionResult (CabalSolution m) u ()
addModuleToConfig pi mi = do
    moduleName <- case mi of
        (ModuleInfo (Symbol name)) -> return $ ModuleName.fromString name
        _ -> throwE $ Unsupported "Cannot add unamed module to a cabal solution"
    p <- lookupCabalProject pi
    p' <- case p of
        LibraryProject lib -> do
            let lib' = lib
                     { exposedModules = exposedModules lib ++ [moduleName] }
            return $ LibraryProject lib'
        _ -> return $ editBuildInfo p $ \bi -> bi{ otherModules = otherModules bi ++ [moduleName] }
    pi' <- getCabalProjectInfo pi
    updateCabalProject pi' p'

-- | Remove a module from a solution's list of modules
removeModuleFromConfig :: (Monad m) 
                       => ProjectInfo 
                       -> ModuleInfo 
                       -> SolutionResult (CabalSolution m) u ()
removeModuleFromConfig pi mi = do
    moduleName <- case mi of
        (ModuleInfo (Symbol name)) -> return $ ModuleName.fromString name
        _ -> throwE $ Unsupported "Cannot add unamed module to a cabal solution"
    pi' <- getCabalProjectInfo pi
    p <- getCabalProject pi'
    let p' = case p of
            LibraryProject lib -> LibraryProject $ lib{ exposedModules = delete moduleName $ exposedModules lib }
            _ -> editBuildInfo p $ \x -> x{ otherModules = delete moduleName $ otherModules x }
    updateCabalProject pi' p'
{- do
    fsp <- lift getFsp
    case fsp of
        Opened (Just (CabalSolutionInfo path cabalConfig)) -> do
            let cabalConfig' = removeModuleFromConfig' m cabalConfig
            lift $ putFsp $ Opened $ Just $ CabalSolutionInfo path cabalConfig
        _ -> throwE $ InvalidOperation "No solution to remove from" ""
-}

writeModulesAndConfig :: ( MonadIO m
                         , ProjectModuleClass m
                         , ProjectExternModuleClass m
                         ) 
                      => SolutionResult (CabalSolution m) u ()
writeModulesAndConfig = do
    fsp <- lift getFsp
    case fsp of
        Opened (Just si) -> do
            let cabalConfigOutput = reformCabalConfiguration $ cabalConfig si
            cabalConfigPath <- getCabalConfigPath $ solutionPath si 
            wrapIOError $ writeFile cabalConfigPath cabalConfigOutput
            let writeProject ptype = do
                    let pi = getProjectInfo ptype
                    moduleInfos <- getModules pi
                    modules <- mapM (getModule pi) moduleInfos
                    mapM_ (writeModule pi) modules
            getCabalProjects >>= mapM_ (getCabalProject >=> writeProject)
        _ -> throwE $ InvalidOperation "No solution to save" ""


instance ( MonadIO m
         , SolutionClass m
         , ProjectModuleClass m
         , ProjectExternModuleClass m
         ) => PersistenceClass (CabalSolution m) where
    -- | Set up an empty solution
    new i = do
        lift $ putFsp $ Opened (Nothing)
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
                lift $ putFsp $ Opened $ Just $ CabalSolutionInfo cabalDirectory cabalConfig Map.empty
                let solutionName = takeBaseName path
                    loadProject' ptype = do
                            let pi = getProjectInfo ptype
                            p <- loadProject pi
                            return (pi,p) 
                projects <- liftM Map.fromList $ getCabalProjects >>= mapM (getCabalProject >=> loadProject')
                --lift $ putSolution $ Solution (SolutionInfo solutionName) projects
                liftIO $ putStrLn "Loaded"
                return ()
            Unopened -> throwE $ InvalidOperation "No path specified for opening" ""
            Opened Nothing -> throwE $ InvalidOperation "Cannot re-open a digested solution" ""
            Opened (Just info) -> do
                lift $ putFsp $ ToOpen $ solutionPath info
                load
    -- | Write out the .cabal file and each of its modules
    finalize = writeModulesAndConfig


instance (MonadIO m, SolutionClass m) => SolutionClass (CabalSolution m) where
    -- | TODO
    editSolutionInfo f = bounce $ editSolutionInfo f
    
    addProject pji = bounce $ addProject pji
    removeProject pji = bounce $ removeProject pji
    getProjects = bounce getProjects
    editProjectInfo pji f = bounce $ editProjectInfo pji f

instance (MonadIO m, ProjectModuleClass m) => ProjectModuleClass (CabalSolution m) where

    addModule pji m = do
        bounce $ addModule pji m
        addModuleToConfig pji $ Module.info m

    createModule pji mi = do
        bounce $ createModule pji mi
        addModuleToConfig pji mi

    getModule pji mi = bounce $ getModule pji mi
    getModules pji = bounce $ getModules pji
    editModule pji mi f = bounce $ editModule pji mi f -- TODO: Check for module info change
    removeModule pji mi = do
        bounce $ removeModule pji mi
        removeModuleFromConfig pji mi

instance (MonadIO m, ProjectExternModuleClass m) => ProjectExternModuleClass (CabalSolution m) where
    addExternModule pji m = bounce $ addExternModule pji m
    getExternModule pji mi = bounce $ getExternModule pji mi
    getExternModules pji = bounce $ getExternModules pji
    removeExternModule pji mi = bounce $ removeExternModule pji mi


instance (MonadIO m, ModuleDeclarationClass m) => ModuleDeclarationClass (CabalSolution m) where
    addDeclaration pji mi d = bounce $ addDeclaration pji mi d
    getDeclaration pji mi di = bounce $ getDeclaration pji mi di
    getDeclarations a b = bounce $ getDeclarations a b
    editDeclaration a b c d = bounce $ editDeclaration a b c d
    removeDeclaration a b c = bounce $ removeDeclaration a b c

instance (MonadIO m, ModuleImportClass m) => ModuleImportClass (CabalSolution m) where
    addImport a b c = bounce $ addImport a b c
    getImport a b c = bounce $ getImport a b c
    removeImport a b c = bounce $ removeImport a b c
    getImports a b = bounce $ getImports a b
    
instance (MonadIO m, ModuleExportClass m) => ModuleExportClass (CabalSolution m) where
    addExport a b c = bounce $ addExport a b c 
    getExport a b c = bounce $ getExport a b c
    removeExport a b c = bounce $ removeExport a b c 
    exportAll a b = bounce $ exportAll a b
    exportNothing a b = bounce $ exportNothing a b
    getExports a b = bounce $ getExports a b

instance (MonadIO m, ModulePragmaClass m) => ModulePragmaClass (CabalSolution m) where
    addPragma a b c = bounce $ addPragma a b c
    removePragma a b c = bounce $ removePragma a b c
    getPragmas a b = bounce $ getPragmas a b


instance ( MonadIO m
         , SolutionClass m
         , ProjectModuleClass m
         , ProjectExternModuleClass m
         ) => ViewerMonad (CabalSolution m) where
    -- | Set the Read file to be opened
    setFileToOpen path = lift $ putFsp $ ToOpen path
    -- | Set the path to be digested
    setDirectoryToOpen path = lift $ putFsp $ ToOpen path
    -- | Set the path to Show to
    setTargetPath path = withOpenedSolution $ \_ -> lift $ do
        modifyFsp $ \case
            Opened (Just info) -> Opened $ Just info{solutionPath = path}
    -- | Check if either there is a new solution, digested path, or Read'd file
    hasOpenedSolution = do
        fsp <- getFsp
        case fsp of
            Opened _ -> return True
            _ -> return False
    createNewFile path = do
        lift $ putFsp $ Opened Nothing
        setTargetPath path
    createNewDirectory path = do
        lift $ putFsp $ Opened Nothing
        setTargetPath path
    prepareBuild = writeModulesAndConfig


instance PseudoStateT CabalSolution FileSystemSolution where
    runPseudoStateT = runStateT . runCabalSolutionInternal


instance (Monad m) => CabalMonad (CabalSolution m) u where
    getCabalProjects = do
        fsp <- lift getFsp
        case fsp of
            Opened (Just info) -> do
                let (CabalConfiguration conf) = cabalConfig info
                    exes = map (ExecutableInfo . fst) $ condExecutables conf
                    tests = map (TestSuiteInfo . fst) $ condTestSuites conf
                    benchs = map (BenchmarkInfo . fst) $ condBenchmarks conf
                    nonlibs = exes ++ tests ++ benchs
                return $ case condLibrary conf of
                    Just _ -> LibraryInfo : nonlibs
                    Nothing -> nonlibs
            _ -> throwE $ InvalidOperation "No solution opened" ""
    getCabalProject i = do
        fsp <- lift getFsp
        case fsp of
            Opened (Just info) -> do
                let (CabalConfiguration conf) = cabalConfig info
                    exes = Map.fromList $ condExecutables conf
                    tests = Map.fromList $ condTestSuites conf
                    benches = Map.fromList $ condBenchmarks conf
                case i of
                    LibraryInfo -> case condLibrary conf of
                        Just lib -> return $ LibraryProject $ condTreeData lib
                        Nothing -> throwE $ ProjectNotFound libraryInfo ""
                    ExecutableInfo s -> case Map.lookup s exes of
                        Just exe -> return $ ExecutableProject (ProjectInfo s) $ condTreeData exe
                        Nothing -> throwE $ ProjectNotFound (ProjectInfo s) ""
                    TestSuiteInfo s -> case Map.lookup s tests of
                        Just test -> return $ TestSuiteProject (ProjectInfo s) $ condTreeData test
                        Nothing -> throwE $ ProjectNotFound (ProjectInfo s) ""
                    BenchmarkInfo s -> case Map.lookup s benches of
                        Just bench -> return $ BenchmarkProject (ProjectInfo s) $ condTreeData bench
                        Nothing -> throwE $ ProjectNotFound (ProjectInfo s) ""
            _ -> throwE $ InvalidOperation "No solution opened" ""
    addCabalProject i p = withOpenedSolution $ \info -> do
        let (CabalConfiguration conf) = cabalConfig info
            path = solutionPath info
            exes = Map.fromList $ condExecutables conf
            tests = Map.fromList $ condTestSuites conf
            benches = Map.fromList $ condBenchmarks conf
        conf' <- case (i,p) of
            (LibraryInfo, LibraryProject lib) -> do
                case condLibrary conf of
                    Just _ -> throwE $ DuplicateProject libraryInfo ""
                    Nothing -> return $ conf{condLibrary = Just $ CondNode
                            { condTreeData = lib
                            , condTreeConstraints = []
                            , condTreeComponents = []
                            }}
            (ExecutableInfo s, ExecutableProject _ exe) -> do
                case Map.lookup s exes of
                    Just _ -> throwE $ DuplicateProject (ProjectInfo s) ""
                    Nothing -> do
                        let exe' = CondNode{ condTreeData = exe
                                           , condTreeConstraints = []
                                           , condTreeComponents = []
                                           }
                            exes' = Map.toList $ Map.insert s exe' exes
                        return conf{condExecutables = exes'}
            (TestSuiteInfo s, TestSuiteProject _ test) -> do
                case Map.lookup s tests of
                    Just _ -> throwE $ DuplicateProject (ProjectInfo s) ""
                    Nothing -> do
                        let test' = CondNode{ condTreeData = test
                                           , condTreeConstraints = []
                                           , condTreeComponents = []
                                           }
                            tests' = Map.toList $ Map.insert s test' tests
                        return conf{ condTestSuites = tests' }
            (BenchmarkInfo s, BenchmarkProject _ bench) -> do
                case Map.lookup s benches of
                    Just _ -> throwE $ DuplicateProject (ProjectInfo s) ""
                    Nothing -> do
                        let bench' = CondNode{ condTreeData = bench
                                             , condTreeConstraints = []
                                             , condTreeComponents = []
                                             }
                            benches' = Map.toList $ Map.insert s bench' benches
                        return $ conf{condBenchmarks = benches'}
        lift $ putFsp $ Opened $ Just $ info{ cabalConfig = CabalConfiguration conf' }
    lookupCabalProject i@(ProjectInfo s) = withOpenedSolution $ \info -> do
        let (CabalConfiguration conf) = cabalConfig info
            exes = Map.fromList $ condExecutables conf
            tests = Map.fromList $ condTestSuites conf
            benches = Map.fromList $ condBenchmarks conf
            exe = Map.lookup s exes
            test = Map.lookup s tests
            bench = Map.lookup s benches
            lib = condLibrary conf
        if i == libraryInfo
            then case lib of
                Just node -> return $ LibraryProject $ condTreeData node
                Nothing -> throwE $ ProjectNotFound i ""
            else case exe of
                Just node -> return $ ExecutableProject i $ condTreeData node
                Nothing -> case test of
                    Just node -> return $ TestSuiteProject i $ condTreeData node
                    Nothing -> case bench of
                        Just node -> return $ BenchmarkProject i $ condTreeData node
                        Nothing -> throwE $ ProjectNotFound i ""
    updateCabalProject i p = withOpenedSolution $ \info -> do
        let (CabalConfiguration conf) = cabalConfig info
            path = solutionPath info
            exes = Map.fromList $ condExecutables conf
            tests = Map.fromList $ condTestSuites conf
            benches = Map.fromList $ condBenchmarks conf
        conf' <- case (i,p) of
            (LibraryInfo, LibraryProject lib) -> do
                case condLibrary conf of
                    Just node -> do
                        let lib' = node{ condTreeData = lib }
                        return conf{ condLibrary = Just lib' }
                    Nothing -> throwE $ ProjectNotFound libraryInfo ""
            (ExecutableInfo s, ExecutableProject _ exe) -> do
                case Map.lookup s exes of
                    Just node -> do
                        let exe' = node{ condTreeData = exe }
                            exes' = Map.toList $ Map.insert s exe' exes
                        return conf{ condExecutables = exes' }
                    Nothing -> throwE $ ProjectNotFound (ProjectInfo s) ""
            (TestSuiteInfo s, TestSuiteProject _ test) -> do
                case Map.lookup s tests of
                    Just node -> do
                        let test' = node{ condTreeData = test }
                            tests' = Map.toList $ Map.insert s test' tests
                        return $ conf{ condTestSuites = tests' }
                    Nothing -> throwE $ ProjectNotFound (ProjectInfo s) ""
            (BenchmarkInfo s, BenchmarkProject _ bench) -> do
                case Map.lookup s benches of
                    Just node -> do
                        let bench' = node{ condTreeData = bench }
                            benches' = Map.toList $ Map.insert s bench' benches
                        return $ conf{ condBenchmarks = benches' }
                    Nothing -> throwE $ ProjectNotFound (ProjectInfo s) ""
        lift $ putFsp $ Opened $ Just $ info{ cabalConfig = CabalConfiguration conf' }
    getCabalProjectInfo i@(ProjectInfo s) = withOpenedSolution $ \info -> do
        let (CabalConfiguration conf) = cabalConfig info
            exes = Map.fromList $ condExecutables conf
            tests = Map.fromList $ condTestSuites conf
            benches = Map.fromList $ condBenchmarks conf
            exe = Map.lookup s exes
            test = Map.lookup s tests
            bench = Map.lookup s benches
            lib = condLibrary conf
        if i == libraryInfo
            then case lib of
                Just _ -> return $ LibraryInfo
                Nothing -> throwE $ ProjectNotFound i ""
            else case exe of
                Just _ -> return $ ExecutableInfo s
                Nothing -> case test of
                    Just _ -> return $ TestSuiteInfo s
                    Nothing -> case bench of
                        Just _ -> return $ BenchmarkInfo s
                        Nothing -> throwE $ ProjectNotFound i ""
    
