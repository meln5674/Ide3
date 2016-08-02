{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-|
Module      : Main
Description : Demo solution main module
Copyright   : (c) Andrew Melnick, 2016

License     : BSD3
Maintainer  : meln5674@kettering.edu
Stability   : experimental
Portability : POSIX

This is the main module for the demo haskell IDE solution
-}
module Main where

import Data.Proxy

import Control.Monad.Catch

import Control.Monad.Trans
import Control.Monad.Trans.State.Strict
import Control.Monad

import System.Console.Haskeline

import Ide3.NewMonad 
import Ide3.NewMonad.Instances.State.Class
import Ide3.NewMonad.Instances.Undecidable
import Ide3.NewMonad.Instances.State.Class.Instances.Strict

import qualified Ide3.Solution as Solution

import Command.Trans
import Command.Types
import Command
import Viewer
import qualified ReadOnlyFilesystemSolution as RDONLY
import qualified SimpleFilesystemSolution as RDWR
import qualified CabalFilesystemSolution as Cabal

import Args
import Editor
import Builder
import Runner
import Initializer
import ProjectInitializer
import ProjectInitializer.Cabal

import CabalMonad

{-
deriving instance (MonadException m) => MonadException (SolutionStateT m)
deriving instance (MonadException m) => MonadException (StatefulSolution m)
-}

deriving instance (MonadMask m) => MonadMask (StatefulWrapper m)
deriving instance (MonadCatch m) => MonadCatch (StatefulWrapper m)
deriving instance (MonadThrow m) => MonadThrow (StatefulWrapper m)
deriving instance (MonadException m) => MonadException (StatefulWrapper m)

deriving instance (MonadMask (t m)) => MonadMask (UndecidableWrapper t m)
deriving instance (MonadCatch (t m)) => MonadCatch (UndecidableWrapper t m)
deriving instance (MonadThrow (t m)) => MonadThrow (UndecidableWrapper t m)
deriving instance (MonadException (t m)) => MonadException (UndecidableWrapper t m)

deriving instance (MonadMask m) => MonadMask (SolutionStateT m)
deriving instance (MonadCatch m) => MonadCatch (SolutionStateT m)
deriving instance (MonadThrow m) => MonadThrow (SolutionStateT m)
deriving instance (MonadException m) => MonadException (SolutionStateT m)

deriving instance (MonadMask m) => MonadMask (ViewerStateT m)
deriving instance (MonadCatch m) => MonadCatch (ViewerStateT m)
deriving instance (MonadThrow m) => MonadThrow (ViewerStateT m)
deriving instance (MonadException m) => MonadException (ViewerStateT m)

deriving instance (MonadException m) => MonadException (Cabal.CabalSolution m)
deriving instance (MonadException m) => MonadException (RDWR.SimpleFilesystemSolutionT m)

deriving instance (MonadMask m) => MonadMask (RDONLY.ReadOnlyFilesystemSolutionT m)
deriving instance (MonadCatch m) => MonadCatch (RDONLY.ReadOnlyFilesystemSolutionT m)
deriving instance (MonadThrow m) => MonadThrow (RDONLY.ReadOnlyFilesystemSolutionT m)
deriving instance (MonadException m) => MonadException (RDONLY.ReadOnlyFilesystemSolutionT m)


-- | Run a single iteration of reading input from the user, deciding which command to run,
-- running it, then printing the response, and indicating if the user wishes to quit
repl :: (MonadException m, ViewerAction m u) 
     => InputT (CommandT () (ViewerStateT m)) Bool
repl = do
    input <- getInputLine ">"
    case input of
        Nothing -> return True
        Just inputCmd -> do
            response <- lift $ execCommand inputCmd ()
            outputStrLn response
            lift getExitFlag

-- | Run the main program loop forever until the user indicates they wish to quit
runMain :: (MonadException m, ViewerAction m u) 
        => InputT (CommandT () (ViewerStateT m)) ()
runMain = do
    continue <- repl
    when continue runMain

-- | Settings for the line editor transformer
settings :: (MonadException m, ViewerAction m u)
         => Settings (CommandT () (ViewerStateT m))
settings = Settings{complete=cmdCompletion, historyFile=Nothing, autoAddHistory=True}

-- | List of commands, using a specified editor
commandList :: ( {-?mProxy :: Proxy m
               ,-} Monad m
               , MonadMask m
               , ViewerIOAction m u
               , Args a
               , Args pa
               , PersistenceClass m
               , SolutionClass m
               , ProjectModuleClass m
               , ProjectExternModuleClass m
               , ModuleDeclarationClass m
               , ModuleImportClass m
               , ModuleExportClass m
               , ModulePragmaClass m
               )
            => Editor m u
            -> Builder m u
            -> Runner m u
            -> Initializer a m u
            -> ProjectInitializer pa m u
            -> [Command () (ViewerStateT m)]
commandList editor builder runner initializer projectInitializer =
    [ helpCmd
    , cdCmd
    , shellCmd
    , newCmd initializer
    , openCmd
    , saveAsCmd
    , saveCmd
    
    , projectsCmd
    , projectCmd
    , addProjectCmd projectInitializer
    , removeProjectCmd

    , modulesCmd
    , moduleCmd
    , addModuleCmd
    , removeModuleCmd
    
    , importsCmd
    , importedCmd
    , addImportCmd
    , removeImportCmd
    
    , exportsCmd
    , exportedCmd
    , addExportCmd
    , removeExportCmd
    
    , declarationsCmd
    , addDeclarationCmd editor
    , removeDeclarationCmd
    
    , visibleCmd
    , catCmd
    , editCmd editor
    , buildCmd builder
    , runCmd runner
    , treeCmd
    , searchCmd
    , quitCmd
    ]


-- | Run the main program using the specified persistance mechanism and editor
runWith :: ( {-?mProxy :: Proxy m
           ,-} Monad m
           , MonadException (t m)
           , MonadMask (t m)
           , ViewerAction (t m) u
           , Monad (t m)
           , Args a
           , Args pa
           , PersistenceClass (t m)
           , SolutionClass (t m)
           , ProjectModuleClass (t m)
           , ProjectExternModuleClass (t m)
           , ModuleDeclarationClass (t m)
           , ModuleImportClass (t m)
           , ModuleExportClass (t m)
           , ModulePragmaClass (t m)
           )
        => (forall b . t m b -> fsp -> m (b, fsp))
        -> fsp 
        -> Editor (t m) u
        -> Builder (t m) u
        -> Runner (t m) u
        -> Initializer a (t m) u
        -> ProjectInitializer pa (t m) u
        -> m ()
runWith runFspT 
        unopened 
        editor 
        builder 
        runner 
        initializer 
        projectInitializer
  = void
  $ flip runFspT unopened
  $ flip runViewerStateT emptyViewer 
  $ flip runCommandT (commandList editor builder runner initializer projectInitializer)
  $ runInputT settings
  $ do
        outputStrLn "Haskell solution viewer"
        outputStrLn "Type \"help\" for commands"
        runMain

-- | Data type which contains options for running the application
data {- (Monad (t (StateT Solution IO))) => -} AppSetup a pa t fsp m u
    = AppSetup
    { -- | The editor to use
      appEditor :: Editor (t m) u
      -- | The builder to use
    , appBuilder :: Builder (t m) u
      -- | The runner to use
    , appRunner :: Runner (t m) u
      -- | The initializer to use
    , appInitializer :: Initializer a (t m) u
      -- | The project initializer to use
    , appProjectInitializer :: ProjectInitializer pa (t m) u
      -- | Function for running the persistence mechanism
    , appRunFspT :: forall b . t m b -> fsp -> m (b, fsp)
      -- | Initial state of the persistence mechanism
    , appUnopened :: fsp
    }

-- | Run the main program with the specified setup
runWithSetup :: ( {-?mProxy :: Proxy m
                ,-} Monad m
                , MonadException (t m)
                , MonadMask (t m)
                , ViewerAction (t m) u
                , Monad (t m)
                , Args a
                , Args pa
                , PersistenceClass (t m)
                , SolutionClass (t m)
                , ProjectModuleClass (t m)
                , ProjectExternModuleClass (t m)
                , ModuleDeclarationClass (t m)
                , ModuleImportClass (t m)
                , ModuleExportClass (t m)
                , ModulePragmaClass (t m)
                )
             => AppSetup a pa t fsp m u
             -> m ()
runWithSetup setup = runWith 
    (appRunFspT setup) 
    (appUnopened setup) 
    (appEditor setup) 
    (appBuilder setup) 
    (appRunner setup) 
    (appInitializer setup)
    (appProjectInitializer setup)

-- | Change a setup to use the nano editor
useNanoEditor :: ( MonadIO (t m)
                 , MonadMask (t m)
                 , Monad (t m)
                 ) 
               => AppSetup a pa t fsp m u -> AppSetup a pa t fsp m u
useNanoEditor s = s{appEditor = nanoEditor}

useStackBuilder :: ( MonadIO (t m)
                   , MonadMask (t m)
                   , Monad (t m)
                   ) 
                => AppSetup a pa t fsp m u -> AppSetup a pa t fsp m u
useStackBuilder s = s{appBuilder = stackBuilder}

useStackRunner :: ( MonadIO (t m)
                  , MonadMask (t m)
                  , Monad (t m)
                  ) 
               => AppSetup a pa t fsp m u -> AppSetup a pa t fsp m u
useStackRunner s = s{appRunner = stackRunner}

useStackInitializer :: ( Monad m
                       , MonadIO (t m)
                       , MonadMask (t m)
                       , Monad (t m)
                       , SolutionClass (t m)
                       , ProjectModuleClass (t m)
                       , ProjectExternModuleClass (t m)
                       , CabalMonad (t m) u
                       ) 
                    => AppSetup a pa t fsp m u -> AppSetup StackInitializerArgs pa t fsp m u
useStackInitializer s = s{appInitializer = stackInitializer}

useStackProjectInitializer :: ( Monad m
                              , MonadIO (t m)
                              , MonadMask (t m)
                              , Monad (t m)
                              , PersistenceClass (t m)
                              , CabalMonad (t m) u
                              ) 
                           => AppSetup a pa t fsp m u -> AppSetup a StackProjectInitializerArgs t fsp m u
useStackProjectInitializer s = s{appProjectInitializer = stackProjectInitializer}


-- | Change a setup to use the read-only persistence mechanism
useReadOnlyFilesystemSolution :: MonadIO m
                              => ( forall t . Monad (t m)
                              => AppSetup a pa t fsp m u )
                              -> AppSetup a pa RDONLY.ReadOnlyFilesystemSolutionT RDONLY.FileSystemSolution m u
useReadOnlyFilesystemSolution s = s
    { appRunFspT = RDONLY.runReadOnlyFilesystemSolutionT
    , appUnopened = RDONLY.Unopened
    }

-- | Change a setup to use the simple persistence mechanism
useSimpleFilesystemSolution :: MonadIO m
                            => ( forall t . Monad (t m)
                            => AppSetup a pa t fsp m u )
                            -> AppSetup a pa RDWR.SimpleFilesystemSolutionT RDWR.FileSystemSolution m u
useSimpleFilesystemSolution s = s
    { appRunFspT = RDWR.runSimpleFilesystemSolutionT
    , appUnopened = RDWR.Unopened
    }

useCabalFilesystemSolution :: MonadIO m
                           => ( forall t . Monad (t m)
                           => AppSetup a pa t fsp m u )
                           -> AppSetup a pa Cabal.CabalSolution Cabal.FileSystemSolution m u
useCabalFilesystemSolution s = s
    { appRunFspT = Cabal.runCabalSolution
    , appUnopened = Cabal.Unopened
    }
-- | A setup with all fields set to bottom
blankSetup :: (Monad (t m)) => AppSetup a pa t fsp m u
blankSetup = AppSetup
           { appRunFspT = error "No file system solution defined"
           , appUnopened = error "No default solution state defined"
           , appEditor = noEditor
           , appBuilder = noBuilder
           , appRunner = noRunner
           , appInitializer = noInitializer
           , appProjectInitializer = noProjectInitializer
           }

-- | Entry point
main :: IO ()
main = flip evalStateT Solution.empty
    $ runSolutionStateT
    $ runStatefulWrapper
    $ runWithSetup
    $ useStackBuilder 
    $ useStackRunner
    $ useStackInitializer
    $ useStackProjectInitializer
    $ useNanoEditor 
    $ useCabalFilesystemSolution 
      blankSetup
  where 
    ?proxy = undefined :: Proxy ()
    
