{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
module CabalFilesystemSolution.Internal where

import qualified Data.Text as T
import qualified Data.Text.IO as T

import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Monoid

import System.Directory
import System.FilePath

import Control.Monad.Catch

import Control.Monad
import Control.Monad.Base
import Control.Monad.Trans
import Control.Monad.Trans.Except
import Control.Monad.State.Strict
import Control.Monad.Trans.Resource

import qualified Distribution.ModuleName as ModuleName
import Distribution.Package
import Distribution.PackageDescription
import Distribution.PackageDescription.Parse
import Distribution.PackageDescription.PrettyPrint

import Ide3.Types
import Ide3.Utils
import Ide3.NewMonad
import Ide3.NewMonad.Utils


import qualified Ide3.Module as Module



import ViewerMonad
import PseudoState
import CabalMonad
import DirtyModuleClass (DirtyModuleClass)
import qualified DirtyModuleClass

-- | State of the mechanism
data FileSystemSolution
    -- | No solution opened
    = Unopened
    -- | A digested path or dump file is opened if Just, a new solution if Nothing
    | Opened (Maybe CabalSolutionInfo)

-- | Path to a cabal directory and the configuration
data CabalSolutionInfo = CabalSolutionInfo
    { solutionPath :: FilePath
    , cabalConfig :: CabalConfiguration
    , modulePathMap :: ModulePathMap
    , dirtyModules :: Set (ProjectInfo, ModuleInfo)
    , cabalFileDirty :: Bool
    }

-- | Map from modules to file paths, used to keep track of which modules came
-- from which source directory
type ModulePathMap = Map (ProjectInfo,ModuleInfo) FilePath

-- | Wrapper around the cabal solution configuration
newtype CabalConfiguration = CabalConfiguration { getCabalConfiguration :: GenericPackageDescription } deriving Show

addModulePath :: ProjectInfo 
              -> ModuleInfo 
              -> FilePath 
              -> CabalSolutionInfo
              -> CabalSolutionInfo
addModulePath pji mi p conf = conf { modulePathMap = Map.insert (pji,mi) p $ modulePathMap conf }

setModuleDirty :: Monad m 
               => ProjectInfo
               -> ModuleInfo 
               -> SolutionResult u (CabalSolution m) ()
setModuleDirty pji mi = withOpenedSolution $ \info -> do
    lift $ putFsp $ Opened $ Just info{ dirtyModules = Set.insert (pji, mi) $ dirtyModules info }

setModulesClean :: Monad m => SolutionResult u (CabalSolution m) ()
setModulesClean = withOpenedSolution $ \info -> do
    lift $ putFsp $ Opened $ Just info{ dirtyModules = Set.empty }

isModuleDirty :: Monad m 
              => ProjectInfo
              -> ModuleInfo
              -> SolutionResult u (CabalSolution m) Bool
isModuleDirty pji mi = withOpenedSolution $ \info -> do
    return $ (pji,mi) `Set.member` dirtyModules info
    
setCabalFileDirty :: Monad m => SolutionResult u (CabalSolution m) ()
setCabalFileDirty = withOpenedSolution $ \info -> do
    lift $ putFsp $ Opened $ Just info{ cabalFileDirty = True }

setCabalFileClean :: Monad m => SolutionResult u (CabalSolution m) ()
setCabalFileClean = withOpenedSolution $ \info -> do
    lift $ putFsp $ Opened $ Just info{ cabalFileDirty = False }

isCabalFileDirty :: Monad m => SolutionResult u (CabalSolution m) Bool
isCabalFileDirty = withOpenedSolution $ return . cabalFileDirty 

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

instance MonadTrans CabalSolution where
    lift = CabalSolution . lift

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
                   => (CabalSolutionInfo -> SolutionResult u (CabalSolution m) a)
                  -> SolutionResult u (CabalSolution m) a
withOpenedSolution f = do
    fsp <- lift getFsp
    case fsp of
        Opened (Just si) -> f si
        _ -> throwE $ InvalidOperation "No open solution" ""
{-
-- | Update a solution's library
updateLibrary :: ProjectInfo -> Library -> GenericPackageDescription -> GenericPackageDescription
updateLibrary pji lib desc@GenericPackageDescription{condLibrary=Just condLib}
    = desc{condLibrary=Just condLib{condTreeData = lib}}
updateLibrary pji lib desc
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
             -> SolutionResult u (CabalSolution m) ()
updateConfig pji (LibraryProject lib) = withOpenedSolution $ \path (CabalConfiguration desc) -> do
    let desc' = updateLibrary pji lib desc
    lift $ putFsp $ Opened $ Just $ CabalSolutionInfo path $ CabalConfiguration desc'
updateConfig pji (ExecutableProject exe) = withOpenedSolution $ \path (CabalConfiguration desc) -> do
    let desc' = updateExecutable pji exe desc
    lift $ putFsp $ Opened $ Just $ CabalSolutionInfo path $ CabalConfiguration desc'
-}


{-
isProjectInfo :: ProjectInfo -> ProjectType -> Bool
isProjectInfo pji (LibraryProject _) = pji == libraryInfo
isProjectInfo (ProjectInfo projectName) (ExecutableProject exe) = exeName exe == projectName


getProject :: Monad m => ProjectInfo -> SolutionResult u (CabalSolution m) CabalProject
getProject pji = do
    ps <- getAllProjects
    case filter (isProjectInfo pji) ps of
        [] -> throwE $ ProjectNotFound pji ""
        [p] -> return p
        _ -> throwE $ DuplicateProject pji ""
-}

toModuleName :: ModuleInfo -> ModuleName.ModuleName
toModuleName (ModuleInfo (Symbol s)) = ModuleName.fromString $ T.unpack s
toModuleName (UnamedModule _) = error "Unamed module"

toModuleInfo :: ModuleName.ModuleName -> ModuleInfo
toModuleInfo mn = ModuleInfo $ Symbol $ T.intercalate "." $ map T.pack $ ModuleName.components mn

locateModule :: (MonadIO m) => [FilePath] -> ModuleName.ModuleName -> m (Maybe FilePath)
locateModule paths mn = do
    let possiblePaths = map (</> (ModuleName.toFilePath mn ++ ".hs")) paths
    hits <- forM possiblePaths $ \path -> do
        exists <- liftIO $ doesFileExist path
        if exists
            then return $ Just path
            else return Nothing
    return $ getFirst $ mconcat $ map First hits

loadInternalModule :: ( MonadIO m
                      , ProjectModuleClass m
                      , ModuleDeclarationClass m
                      , ModuleImportClass m
                      , ModuleExportClass m
                      , ModulePragmaClass m
                      )  
                   => ProjectInfo 
                   -> FilePath
                   -> Bool
                   -> SolutionResult u (CabalSolution m) (ModuleInfo, Maybe (SrcLoc,String))
loadInternalModule pji path isMain = do
    let parser = if isMain then Module.parseMain else Module.parse
    contents <- wrapReadFile path
    -- TODO: Update addRawModule to switch on isMain
    bounce $ addRawModule pji contents (Just path)

updateInternalModule :: ( MonadIO m
                        , ProjectModuleClass m
                        , ModuleDeclarationClass m
                        , ModuleImportClass m
                        , ModuleExportClass m
                        , ModulePragmaClass m
                        )  
                     => ProjectInfo 
                     -> FilePath
                     -> Bool
                     -> SolutionResult u (CabalSolution m) (ModuleInfo, Maybe (SrcLoc,String))
updateInternalModule pji path isMain = do
    let parser = if isMain then Module.parseMain else Module.parse
    contents <- wrapReadFile path
    -- TODO: See above
    bounce $ updateRawModule pji contents (Just path)

locateAndLoadInternalModule :: ( MonadIO m 
                               , ProjectModuleClass m
                               , ModuleDeclarationClass m
                               , ModuleImportClass m
                               , ModuleExportClass m
                               , ModulePragmaClass m
                               )
                            => ProjectInfo
                            -> [FilePath]
                            -> Bool
                            -> ModuleName.ModuleName
                            -> SolutionResult u (CabalSolution m) ( ModuleInfo
                                                                  , FilePath
                                                                  , Maybe (SrcLoc, String)
                                                                  )
locateAndLoadInternalModule pji roots isMain mn = do
    locateResult <- locateModule roots mn
    case locateResult of
        Nothing -> throwE $ InvalidOperation ("Can't find module " ++ show mn) ""
        Just path -> do
            (mi, err) <- loadInternalModule pji path isMain
            return (mi,path,err)

getMainModuleName :: Monad m 
                  => ProjectInfo 
                  -> SolutionResult u (CabalSolution m) (Maybe ModuleName.ModuleName)
getMainModuleName pji = do
    p <- lookupCabalProject pji
    let mainModuleName = case p of
            LibraryProject _ -> Nothing
            ExecutableProject _ exe -> Just $ moduleNameFromPath $ modulePath exe
            TestSuiteProject _ test -> case testInterface test of
                TestSuiteExeV10 _ path -> Just $ moduleNameFromPath $ takeBaseName path
                TestSuiteLibV09 _ name -> Just $ name
                _ -> Nothing
            BenchmarkProject _ bench -> case benchmarkInterface bench of
                BenchmarkExeV10 _ path -> Just $ moduleNameFromPath $ takeBaseName path
                _ -> Nothing
    return mainModuleName

moduleNameFromPath :: FilePath -> ModuleName.ModuleName
moduleNameFromPath = ModuleName.fromString 
                   . intercalate "." 
                   . splitDirectories 
                   . dropExtension

-- | Get a list of modules from a cabal solution
loadInternalModules :: ( MonadIO m
                       , ModuleDeclarationClass m
                       , ModuleImportClass m
                       , ModuleExportClass m
                       , ModulePragmaClass m
                       , ProjectModuleClass m
                       )  
                    => ProjectInfo 
                    -> SolutionResult u (CabalSolution m) [(ModuleInfo,FilePath,Maybe (SrcLoc,String))]
loadInternalModules pji = do
    p <- lookupCabalProject pji
    roots <- getProjectSourceRoots p
    let modules = case p of
            LibraryProject lib -> exposedModules lib ++ (otherModules $ libBuildInfo lib)
            _ -> withBuildInfo p otherModules
    mainModuleName <- getMainModuleName pji
    pairs <- forM modules $ locateAndLoadInternalModule pji roots False
    case mainModuleName of
        Just mn -> do
            mainPair <- locateAndLoadInternalModule pji roots True mn
            return $ mainPair : pairs
        Nothing -> return pairs
    
{-
-- | Get the external modules used by a cabal solution. * INCOMPLETE *
getExternalModules :: (MonadIO m) 
                   => ProjectInfo 
                   -> SolutionResult u (CabalSolution m) [ExternModule]
getExternalModules pji = return []
-}
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
getCabalDirectoryAndConfigPath :: (MonadIO m) => FilePath -> SolutionResult u m (FilePath,FilePath)
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
getCabalDirectory :: (MonadIO m) => FilePath -> SolutionResult u m FilePath
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
            
            
            
getCabalConfigPath :: (MonadIO m) => FilePath -> SolutionResult u m FilePath
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
getCabalDirectory :: (MonadIO m) => FilePath -> SolutionResult u m FilePath
getCabalDirectory = liftM fst . getCabalDirectoryAndConfigPath

-- | Given either a cabal solution root or a cabal solution file, get the cabal solution file
getCabalConfigPath :: (MonadIO m) => FilePath -> SolutionResult u m FilePath
getCabalConfigPath = liftM snd . getCabalDirectoryAndConfigPath

-- | Not entirely sure what this is supposed to do
makeSolutionInfo :: FilePath -> CabalConfiguration -> SolutionInfo
makeSolutionInfo _ (CabalConfiguration d) 
    = SolutionInfo 
    $ T.pack
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
getAllProjects :: (Monad m) => SolutionResult u (CabalSolution m) [ProjectType]
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
               , ModuleDeclarationClass m
               , ModuleImportClass m
               , ModuleExportClass m
               , ModulePragmaClass m
               ) 
            => ProjectInfo 
            -> SolutionResult u (CabalSolution m) [(SrcFileLoc,String)]
loadProject pji = do
    addProject pji
    modules <- loadInternalModules pji
    --externModules <- getExternalModules pji
    liftIO $ putStrLn $ "adding project: " ++ show pji
    let addPaths :: StateT CabalSolutionInfo (State [(SrcFileLoc,String)]) ()
        addPaths = forM_ modules $ \(mi,path,err) -> do
            modify $ addModulePath pji mi path
            case err of
                Just (l,msg) -> lift $ modify ((SrcFileLoc path l, msg) :)
                _ -> return ()
    
    {-
    let addPaths = foldr (.) id 
                 $ flip map modules 
                 $ \(mi,path,err) -> do
                    addModulePath pji mi path
                    case err of
    -}
                      
    (Opened (Just info)) <- lift getFsp
    let (info', errs) = runState (execStateT addPaths info) []
    lift $ putFsp $ Opened $ Just info'
    return errs
    --forM_ modules $ bounce . addModule pji . fst -- This bounce is important, 
                                                -- as it targets the
                                                -- underlying monad, instead 
                                                -- of (CabalSolution m),
                                                -- preventing it from adding
                                                -- the module to the config
    --forM_ externModules $ addExternModule pji
    
    --let p = Project.new pji
    --p' <- mapDescent2_ Project.addModule p modules
    --mapDescent2_ Project.addExternModule p' externModules

{-
-- | Get the type of a solution
getProjectType :: (Monad m) => SolutionResult u (CabalSolution m) ProjectType
getProjectType = withOpenedSolution $ \_ (CabalConfiguration desc) -> do
    case condLibrary desc of
        Just CondNode{condTreeData=lib} -> return $ LibraryProject $ lib
        Nothing -> case condExecutables desc of
            [(str,CondNode{condTreeData=exe})] -> return $ ExecutableProject $ exe
            _ -> throwE $ Unsupported "Cabal solution must have 1 and only 1 source dir"
-}

-- | Get the directory that a solution stores its code files in
getProjectSourceRoots :: (Monad m) => CabalProject -> SolutionResult u (CabalSolution m) [FilePath]
getProjectSourceRoots p = withBuildInfo p $ \b -> case hsSourceDirs b of
    [] -> return [""]
    x -> return x

-- | Write a module out to file
writeModule :: ( MonadIO m
               , ModuleFileClass m
               ) 
            => ProjectInfo 
            -> ModuleInfo 
            -> SolutionResult u (CabalSolution m) ()
writeModule pji mi = withOpenedSolution $ \info -> do
    let modulePathLookup = Map.lookup (pji, mi) $ modulePathMap info
    modulePath <- case modulePathLookup of
        Just path -> return path
        Nothing -> throwE $ InternalError ("Can't find source directory for module " ++ show mi) ""
    text <- bounce $ toFile pji mi
    let moduleDir = takeDirectory modulePath
    wrapIOError $ do
        createDirectoryIfMissing True moduleDir
        T.writeFile modulePath text

-- | Add a module to a solution's list of modules
addModuleToConfig :: (Monad m) 
                  => ProjectInfo 
                  -> ModuleInfo 
                  -> SolutionResult u (CabalSolution m) ()
addModuleToConfig pji mi = do
    setCabalFileDirty
    moduleName <- case mi of
        (ModuleInfo (Symbol name)) -> return $ ModuleName.fromString $ T.unpack name
        _ -> throwE $ Unsupported "Cannot add unamed module to a cabal solution"
    p <- lookupCabalProject pji
    p' <- case p of
        LibraryProject lib -> do
            let lib' = lib
                     { exposedModules = exposedModules lib ++ [moduleName] }
            return $ LibraryProject lib'
        _ -> return $ editBuildInfo p $ \bi -> bi{ otherModules = otherModules bi ++ [moduleName] }
    pji' <- getCabalProjectInfo pji
    updateCabalProject pji' p'
    (root:_) <- getProjectSourceRoots p
    (Opened (Just info)) <- lift getFsp
    lift $ putFsp $ Opened $ Just $ addModulePath pji mi (root </> ModuleName.toFilePath moduleName <.> "hs") info

-- | Remove a module from a solution's list of modules
removeModuleFromConfig :: (Monad m) 
                       => ProjectInfo 
                       -> ModuleInfo 
                       -> SolutionResult u (CabalSolution m) ()
removeModuleFromConfig pji mi = do
    setCabalFileDirty
    moduleName <- case mi of
        (ModuleInfo (Symbol name)) -> return $ ModuleName.fromString $ T.unpack name
        _ -> throwE $ Unsupported "Cannot add unamed module to a cabal solution"
    pji' <- getCabalProjectInfo pji
    p <- getCabalProject pji'
    let p' = case p of
            LibraryProject lib -> LibraryProject $ lib{ exposedModules = delete moduleName $ exposedModules lib }
            _ -> editBuildInfo p $ \x -> x{ otherModules = delete moduleName $ otherModules x }
    updateCabalProject pji' p'
{- do
    fsp <- lift getFsp
    case fsp of
        Opened (Just (CabalSolutionInfo path cabalConfig)) -> do
            let cabalConfig' = removeModuleFromConfig' m cabalConfig
            lift $ putFsp $ Opened $ Just $ CabalSolutionInfo path cabalConfig
        _ -> throwE $ InvalidOperation "No solution to remove from" ""
-}

writeModulesAndConfig :: ( MonadIO m
                         , ModuleFileClass m
                         , SolutionMonad m
                         ) 
                      => SolutionResult u (CabalSolution m) ()
writeModulesAndConfig = do
    fsp <- lift getFsp
    case fsp of
        Opened (Just si) -> do
            isDirty <- isCabalFileDirty
            when isDirty $ do
                let cabalConfigOutput = reformCabalConfiguration $ cabalConfig si
                cabalConfigPath <- getCabalConfigPath $ solutionPath si 
                wrapIOError $ writeFile cabalConfigPath cabalConfigOutput
                setCabalFileClean
            let writeProject ptype = do
                    let pji = getProjectInfo ptype
                    moduleInfos <- getModules pji
                    dirtyModuleInfos <- filterM (isModuleDirty pji) moduleInfos
                    mapM_ (writeModule pji) dirtyModuleInfos
                    return ()
            getCabalProjects >>= mapM_ (getCabalProject >=> writeProject)
            setModulesClean
        _ -> throwE $ InvalidOperation "No solution to save" ""


instance ( MonadIO m
         , SolutionMonad m
         , ModuleFileClass m
         ) => PersistenceClass (CabalSolution m) where
    type PersistToken (CabalSolution m) = FilePath
    -- | Set up an empty solution
    new i = do
        lift $ putFsp $ Opened Nothing
        bounce $ editSolutionInfo $ const i
        pjis <- bounce $ getProjects
        forM_ pjis $ bounce . removeProject
    -- | Open a solution by reading the .cabal file and using it to find all of its modules
    load path = do
        pjis <- bounce $ getProjects
        forM_ pjis $ bounce . removeProject
        absPath <- wrapIOError $ makeAbsolute path
        (cabalDirectory,cabalConfigPath) <- getCabalDirectoryAndConfigPath absPath
        wrapIOError $ setCurrentDirectory cabalDirectory
        cabalConfig <- wrapReadFile cabalConfigPath
                            >>= ExceptT . return . parseCabalConfiguration
        lift $ putFsp $ Opened $ Just $ CabalSolutionInfo cabalDirectory cabalConfig Map.empty Set.empty False
        ps <- getCabalProjects
        liftIO $ print ps
        forM_ ps $ getCabalProject >=> loadProject . getProjectInfo

    -- | Write out the .cabal file and each of its modules
    finalize Nothing = writeModulesAndConfig
    finalize (Just newPath) = do
        fsp <- lift getFsp
        case fsp of
            Unopened -> throwE $ InvalidOperation "No solution open" ""
            Opened (Just info) -> do
                pjis <- bounce getProjects
                mis <- forM pjis $ \pji -> do
                    mis <- getModules pji
                    return (pji, mis)
                let mis' = concatMap (\(pji,allMis) -> map (\mi -> (pji,mi)) allMis) mis
                    info' = info{ solutionPath = newPath, dirtyModules = Set.fromList mis' }
                lift $ putFsp $ Opened $ Just info'
                writeModulesAndConfig


instance (MonadIO m, SolutionClass m) => SolutionClass (CabalSolution m) where
    editSolutionInfo f = bounce $ editSolutionInfo f
    
    addProject pji = bounce $ addProject pji
    removeProject pji = bounce $ removeProject pji
    getProjects = bounce getProjects
    editProjectInfo pji f = bounce $ editProjectInfo pji f

instance ( MonadIO m
         , SolutionMonad m
         ) => ProjectModuleClass (CabalSolution m) where
    {-addModule pji m = do
        bounce $ addModule pji m
        addModuleToConfig pji $ Module.info m-}

    createModule pji mi = do
        setModuleDirty pji mi
        bounce $ createModule pji mi
        addModuleToConfig pji mi

    --getModule pji mi = bounce $ getModule pji mi
    getModules pji = bounce $ getModules pji
    --editModule pji mi f = bounce $ editModule pji mi f -- TODO: Check for module info change
    removeModule pji mi = do
        bounce $ removeModule pji mi
        removeModuleFromConfig pji mi
    getModuleHeader pji mi = bounce $ getModuleHeader pji mi
    editModuleHeader pji mi f = do
        setModuleDirty pji mi
        bounce $ editModuleHeader pji mi f
    setModuleUnparsable pji mi contents loc msg = do
        setModuleDirty pji mi
        bounce $ setModuleUnparsable pji mi contents loc msg
    setModuleParsable pji mi = do
        setModuleDirty pji mi
        bounce $ setModuleParsable pji mi
    getUnparsableModule pji mi = bounce $ getUnparsableModule pji mi
    refreshModule pji mi = withOpenedSolution $ \info -> do
        mi' <- bounce $ refreshModule pji mi
        let modulePathLookup = Map.lookup (pji, mi') $ modulePathMap info
        modulePath <- case modulePathLookup of
            Just path -> return path
            Nothing -> throwE $ InternalError ("Can't find source directory for module " ++ show mi) ""
        mainModuleName <- getMainModuleName pji
        let moduleName = moduleNameFromPath modulePath
        (mi'', _) <- updateInternalModule pji modulePath (mainModuleName == Just moduleName)
        return mi''

instance (MonadIO m, ProjectExternModuleClass m) => ProjectExternModuleClass (CabalSolution m) where
    --addExternModule pji m = bounce $ addExternModule pji m
    createExternModule pji m = bounce $ createExternModule pji m
    --getExternModule pji mi = bounce $ getExternModule pji mi
    getExternModules pji = bounce $ getExternModules pji
    removeExternModule pji mi = bounce $ removeExternModule pji mi


instance (MonadIO m, ModuleDeclarationClass m) => ModuleDeclarationClass (CabalSolution m) where
    addDeclaration pji mi d = do
        setModuleDirty pji mi
        bounce $ addDeclaration pji mi d
    getDeclaration pji mi di = bounce $ getDeclaration pji mi di
    getDeclarations a b = bounce $ getDeclarations a b
    editDeclaration a b c d = do
        setModuleDirty a b
        bounce $ editDeclaration a b c d
    removeDeclaration a b c = do
        setModuleDirty a b
        bounce $ removeDeclaration a b c

instance (MonadIO m, ModuleImportClass m) => ModuleImportClass (CabalSolution m) where
    addImport a b c = do
        setModuleDirty a b
        bounce $ addImport a b c
    getImport a b c = bounce $ getImport a b c
    removeImport a b c = do
        setModuleDirty a b
        bounce $ removeImport a b c
    getImports a b = bounce $ getImports a b
    
instance ( MonadIO m, ModuleExportClass m) => ModuleExportClass (CabalSolution m) where
    addExport a b c = do
        setModuleDirty a b
        bounce $ addExport a b c 
    getExport a b c = bounce $ getExport a b c
    removeExport a b c = do
        setModuleDirty a b
        bounce $ removeExport a b c 
    exportAll a b = do
        setModuleDirty a b
        bounce $ exportAll a b
    exportNothing a b = do
        setModuleDirty a b
        bounce $ exportNothing a b
    getExports a b = bounce $ getExports a b

instance (MonadIO m, ModulePragmaClass m) => ModulePragmaClass (CabalSolution m) where
    addPragma a b c = do
        setModuleDirty a b
        bounce $ addPragma a b c
    removePragma a b c = do
        setModuleDirty a b
        bounce $ removePragma a b c
    getPragmas a b = bounce $ getPragmas a b



instance (MonadIO m, ExternModuleExportClass m) => ExternModuleExportClass (CabalSolution m) where
    addExternExport = bounce .-... addExternExport
    getExternExport = bounce .-... getExternExport
    removeExternExport = bounce .-... removeExternExport
    getExternExports = bounce .-.. getExternExports

{-
instance (MonadIO m, ModuleLocationClass m) => ModuleLocationClass (CabalSolution m) where
    getModuleItemAtLocation = bounce .-... getModuleItemAtLocation
-}

instance (MonadIO m, ModuleDeclarationClass m, ModuleImportClass m, ModuleExportClass m) => ModuleLocationClass (CabalSolution m) where
    getModuleItemAtLocation pji mi l = do
        path <- case mi of
            UnamedModule Nothing -> throwE $ InvalidOperation "Can't find an unnamed module" ""
            UnamedModule (Just path) -> return path
            _ -> do
                let name = toModuleName mi
                p <- lookupCabalProject pji
                roots <- getProjectSourceRoots p
                result <- locateModule roots name
                case result of
                    Just path -> return path
                    Nothing -> throwE $ InvalidOperation "Can't find module" ""
        contents <- wrapIOError $ readFile path
        results <- ExceptT $ return $ Module.parseAtLocation l contents (Just path)
        forM results $ \result ->  case result of
            Nothing -> return Nothing
            Just (resultItem, l') -> do
                resultString <- case resultItem of
                    HeaderCommentItem s -> return $ HeaderCommentString s
                    PragmaItem p -> return $ PragmaString p
                    ImportItem i -> do
                        iis <- getImports pji mi
                        is <- forM iis $ \ii -> do
                            i' <- getImport pji mi ii
                            return (ii,i')
                        let is' = filter ((==i) . snd) is
                        case is' of
                            [] -> throwE $ InvalidOperation "Can't find import" ""
                            ((ii,_):_) -> return $ ImportString $ WithBody ii $ body i
                    ExportItem e -> do
                        eis <- liftM (maybe [] id) $ getExports pji mi
                        es <- forM eis $ \ei -> do
                            e' <- getExport pji mi ei
                            return (ei,e')
                        let es' = filter ((==e) . snd) es
                        case es' of
                            [] -> throwE $ InvalidOperation "Can't find export" ""
                            ((ei,_):_) -> return $ ExportString $ WithBody ei $ body e
                    DeclarationItem d -> do
                        dis <- getDeclarations pji mi
                        ds <- forM dis $ \di -> do
                            d' <- getDeclaration pji mi di
                            return (di,d')
                        let ds' = filter ((==d) . snd) ds
                        case ds' of
                            [] -> throwE $ InvalidOperation "Can't find export" ""
                            ((di,_):_) -> return $ DeclarationString $ WithBody di $ body d
                return $ Just (resultString, l')

instance ( MonadIO m
         , SolutionMonad m
         , ModuleFileClass m
         ) => ViewerMonad (CabalSolution m) where
    type ViewerPersistToken (CabalSolution m) = FilePath
    preparePathToken = return
{-
    -- | Set the Read file to be opened
    setFileToOpen path = lift $ putFsp $ ToOpen path
    -- | Set the path to be digested
    setDirectoryToOpen path = lift $ putFsp $ ToOpen path
    -- | Set the path to Show to
    setTargetPath path = withOpenedSolution $ \info -> do
        lift $ putFsp $ Opened $ Just info{ solutionPath = path }
-}
    -- | Check if either there is a new solution, digested path, or Read'd file
    hasOpenedSolution = do
        fsp <- getFsp
        case fsp of
            Opened _ -> return True
            _ -> return False
{-
    createNewFile path = do
        lift $ putFsp $ Opened Nothing
        setTargetPath path
    createNewDirectory path = do
        lift $ putFsp $ Opened Nothing
        setTargetPath path
-}
    prepareBuild = writeModulesAndConfig


instance PseudoStateT CabalSolution FileSystemSolution where
    runPseudoStateT = runStateT . runCabalSolutionInternal


instance (Monad m) => CabalMonad (CabalSolution m) where
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
                        Just exe -> return $ ExecutableProject pji $ condTreeData exe
                        Nothing -> throwE $ ProjectNotFound pji ""
                      where
                        pji = ProjectInfo $ T.pack s
                    TestSuiteInfo s -> case Map.lookup s tests of
                        Just test -> return $ TestSuiteProject pji $ condTreeData test
                        Nothing -> throwE $ ProjectNotFound pji ""
                      where
                        pji = ProjectInfo $ T.pack s
                    BenchmarkInfo s -> case Map.lookup s benches of
                        Just bench -> return $ BenchmarkProject pji $ condTreeData bench
                        Nothing -> throwE $ ProjectNotFound pji ""
                      where
                        pji = ProjectInfo $ T.pack s
            _ -> throwE $ InvalidOperation "No solution opened" ""
    addCabalProject i p = withOpenedSolution $ \info -> do
        let (CabalConfiguration conf) = cabalConfig info
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
                    Just _ -> throwE $ DuplicateProject pji ""
                    Nothing -> do
                        let exe' = CondNode{ condTreeData = exe
                                           , condTreeConstraints = []
                                           , condTreeComponents = []
                                           }
                            exes' = Map.toList $ Map.insert s exe' exes
                        return conf{condExecutables = exes'}
              where
                pji = ProjectInfo $ T.pack s
            (TestSuiteInfo s, TestSuiteProject _ test) -> do
                case Map.lookup s tests of
                    Just _ -> throwE $ DuplicateProject pji ""
                    Nothing -> do
                        let test' = CondNode{ condTreeData = test
                                           , condTreeConstraints = []
                                           , condTreeComponents = []
                                           }
                            tests' = Map.toList $ Map.insert s test' tests
                        return conf{ condTestSuites = tests' }
              where
                pji = ProjectInfo $ T.pack s
            (BenchmarkInfo s, BenchmarkProject _ bench) -> do
                case Map.lookup s benches of
                    Just _ -> throwE $ DuplicateProject pji ""
                    Nothing -> do
                        let bench' = CondNode{ condTreeData = bench
                                             , condTreeConstraints = []
                                             , condTreeComponents = []
                                             }
                            benches' = Map.toList $ Map.insert s bench' benches
                        return $ conf{condBenchmarks = benches'}
              where
                pji = ProjectInfo $ T.pack s
            (_, _) -> throwE $ InternalError "Mismatched project info and contents" ""
        lift $ putFsp $ Opened $ Just $ info{ cabalConfig = CabalConfiguration conf' }
        setCabalFileDirty
    lookupCabalProject i@(ProjectInfo s) = withOpenedSolution $ \info -> do
        let (CabalConfiguration conf) = cabalConfig info
            exes = Map.fromList $ condExecutables conf
            tests = Map.fromList $ condTestSuites conf
            benches = Map.fromList $ condBenchmarks conf
            exe = Map.lookup (T.unpack s) exes
            test = Map.lookup (T.unpack s) tests
            bench = Map.lookup (T.unpack s) benches
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
                    Nothing -> throwE $ ProjectNotFound pji ""
              where
                pji = ProjectInfo $ T.pack s
            (TestSuiteInfo s, TestSuiteProject _ test) -> do
                case Map.lookup s tests of
                    Just node -> do
                        let test' = node{ condTreeData = test }
                            tests' = Map.toList $ Map.insert s test' tests
                        return $ conf{ condTestSuites = tests' }
                    Nothing -> throwE $ ProjectNotFound pji ""
              where
                pji = ProjectInfo $ T.pack s
            (BenchmarkInfo s, BenchmarkProject _ bench) -> do
                case Map.lookup s benches of
                    Just node -> do
                        let bench' = node{ condTreeData = bench }
                            benches' = Map.toList $ Map.insert s bench' benches
                        return $ conf{ condBenchmarks = benches' }
                    Nothing -> throwE $ ProjectNotFound pji ""
              where
                pji = ProjectInfo $ T.pack s
            _ -> throwE $ InternalError "Mismatched project info and type" ""
        lift $ putFsp $ Opened $ Just $ info{ cabalConfig = CabalConfiguration conf' }
        setCabalFileDirty
    getCabalProjectInfo pji = withOpenedSolution $ \info -> do
        let t = unProjectInfo pji
            s = T.unpack t
            (CabalConfiguration conf) = cabalConfig info
            exes = Map.fromList $ condExecutables conf
            tests = Map.fromList $ condTestSuites conf
            benches = Map.fromList $ condBenchmarks conf
            exe = Map.lookup s exes
            test = Map.lookup s tests
            bench = Map.lookup s benches
            lib = condLibrary conf
        if pji == libraryInfo
            then case lib of
                Just _ -> return $ LibraryInfo
                Nothing -> throwE $ ProjectNotFound pji ""
            else case exe of
                Just _ -> return $ ExecutableInfo s
                Nothing -> case test of
                    Just _ -> return $ TestSuiteInfo s
                    Nothing -> case bench of
                        Just _ -> return $ BenchmarkInfo s
                        Nothing -> throwE $ ProjectNotFound pji ""
    removeCabalProject i = withOpenedSolution $ \info -> do
        let (CabalConfiguration conf) = cabalConfig info
            exes = Map.fromList $ condExecutables conf
            tests = Map.fromList $ condTestSuites conf
            benches = Map.fromList $ condBenchmarks conf
        conf' <- case (i) of
            (LibraryInfo) -> do
                case condLibrary conf of
                    Nothing -> throwE $ ProjectNotFound libraryInfo ""
                    _ -> return conf{ condLibrary = Nothing }
            (ExecutableInfo s) -> do
                case Map.lookup s exes of
                    Nothing -> throwE $ ProjectNotFound pji ""
                    _ -> do
                        let exes' = Map.toList $ Map.delete s exes
                        return conf{ condExecutables = exes' }
              where
                pji = ProjectInfo $ T.pack s
            (TestSuiteInfo s) -> do
                case Map.lookup s tests of
                    Nothing -> throwE $ ProjectNotFound pji ""
                    _ -> do
                        let tests' = Map.toList $ Map.delete s tests
                        return $ conf{ condTestSuites = tests' }
              where
                pji = ProjectInfo $ T.pack s
            (BenchmarkInfo s) -> do
                case Map.lookup s benches of
                    Nothing -> throwE $ ProjectNotFound pji ""
                    _ -> do
                        let benches' = Map.toList $ Map.delete s benches
                        return $ conf{ condBenchmarks = benches' }
              where
                pji = ProjectInfo $ T.pack s
        lift $ putFsp $ Opened $ Just $ info{ cabalConfig = CabalConfiguration conf' }
        setCabalFileDirty
    getPackageDescription = withOpenedSolution $ \info -> do
        let CabalConfiguration genericPkg = cabalConfig info 
        return $ packageDescription genericPkg


instance Monad m => DirtyModuleClass (CabalSolution m) where
    isModuleDirty = CabalFilesystemSolution.Internal.isModuleDirty

instance (MonadBase IO m, MonadResource m) => MonadResource (CabalSolution m) where
    liftResourceT = lift . liftResourceT

instance (MonadBase b m) => MonadBase b (CabalSolution m) where
    liftBase = lift . liftBase
    
