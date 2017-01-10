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
import Builder.Stack
import Runner
import Runner.Stack
import Initializer
import Initializer.Stack
import ProjectInitializer
import ProjectInitializer.Stack
import ProjectInitializer.Stack.Types

import CabalMonad

{-
deriving instance (MonadException m) => MonadException (SolutionStateT m)
deriving instance (MonadException m) => MonadException (StatefulSolution m)
-}

deriving instance (MonadMask m) => MonadMask (StatefulWrapper m)
deriving instance (MonadCatch m) => MonadCatch (StatefulWrapper m)
deriving instance (MonadThrow m) => MonadThrow (StatefulWrapper m)
deriving instance (MonadException m) => MonadException (StatefulWrapper m)

deriving instance (MonadMask m) => MonadMask (SolutionStateT m)
deriving instance (MonadCatch m) => MonadCatch (SolutionStateT m)
deriving instance (MonadThrow m) => MonadThrow (SolutionStateT m)
deriving instance (MonadException m) => MonadException (SolutionStateT m)

deriving instance (MonadException m) => MonadException (ViewerStateT m)

deriving instance (MonadException m) => MonadException (Cabal.CabalSolution m)
deriving instance (MonadException m) => MonadException (RDWR.SimpleFilesystemSolutionT m)

deriving instance (MonadMask m) => MonadMask (RDONLY.ReadOnlyFilesystemSolutionT m)
deriving instance (MonadCatch m) => MonadCatch (RDONLY.ReadOnlyFilesystemSolutionT m)
deriving instance (MonadThrow m) => MonadThrow (RDONLY.ReadOnlyFilesystemSolutionT m)
deriving instance (MonadException m) => MonadException (RDONLY.ReadOnlyFilesystemSolutionT m)


-- | Run a single iteration of reading input from the user, deciding which command to run,
-- running it, then printing the response, and indicating if the user wishes to quit
repl :: (MonadException m) 
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
runMain :: (MonadException m, ViewerAction m) 
        => InputT (CommandT () (ViewerStateT m)) ()
runMain = do
    continue <- repl
    when continue runMain

-- | Settings for the line editor transformer
settings :: (MonadException m)
         => Settings (CommandT () (ViewerStateT m))
settings = Settings{complete=cmdCompletion, historyFile=Nothing, autoAddHistory=True}

-- | List of commands, using a specified editor
commandList :: ( ViewerIOAction m
               , Args a
               , Args pa
               , PersistenceClass m
               )
            => Editor m
            -> Builder m
            -> Runner m
            -> Initializer a m
            -> ProjectInitializer pa m
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
runWith :: ( Monad m
           , MonadException (t m)
           , ViewerAction (t m)
           , Args a
           , Args pa
           , PersistenceClass (t m)
           )
        => (forall b . t m b -> fsp -> m (b, fsp))
        -> fsp 
        -> Editor (t m)
        -> Builder (t m)
        -> Runner (t m)
        -> Initializer a (t m)
        -> ProjectInitializer pa (t m)
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
data {- (Monad (t (StateT Solution IO))) => -} AppSetup a pa t fsp m
    = AppSetup
    { -- | The editor to use
      appEditor :: Editor (t m)
      -- | The builder to use
    , appBuilder :: Builder (t m)
      -- | The runner to use
    , appRunner :: Runner (t m)
      -- | The initializer to use
    , appInitializer :: Initializer a (t m)
      -- | The project initializer to use
    , appProjectInitializer :: ProjectInitializer pa (t m)
      -- | Function for running the persistence mechanism
    , appRunFspT :: forall b . t m b -> fsp -> m (b, fsp)
      -- | Initial state of the persistence mechanism
    , appUnopened :: fsp
    }

-- | Run the main program with the specified setup
runWithSetup :: ( Monad m
                , MonadException (t m)
                , ViewerAction (t m)
                , Args a
                , Args pa
                , PersistenceClass (t m)
                )
             => AppSetup a pa t fsp m
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
                 ) 
               => AppSetup a pa t fsp m -> AppSetup a pa t fsp m
useNanoEditor s = s{appEditor = nanoEditor}

useStackBuilder :: ( MonadIO (t m)
                   , MonadMask (t m)
                   , CabalMonad (t m)
                   ) 
                => AppSetup a pa t fsp m -> AppSetup a pa t fsp m
useStackBuilder s = s{appBuilder = stackBuilder}

useStackRunner :: ( MonadIO (t m)
                  , MonadMask (t m)
                  , CabalMonad (t m)
                  ) 
               => AppSetup a pa t fsp m -> AppSetup a pa t fsp m
useStackRunner s = s{appRunner = stackRunner}

useStackInitializer :: ( MonadIO (t m)
                       ) 
                    => AppSetup a pa t fsp m -> AppSetup StackInitializerArgs pa t fsp m
useStackInitializer s = s{appInitializer = stackInitializer}

useStackProjectInitializer :: ( MonadIO (t m)
                              , CabalMonad (t m)
                              ) 
                           => AppSetup a pa t fsp m -> AppSetup a StackProjectInitializerArgs' t fsp m
useStackProjectInitializer s = s{appProjectInitializer = stackProjectInitializer}


-- | Change a setup to use the read-only persistence mechanism
useReadOnlyFilesystemSolution :: MonadIO m
                              => ( forall t . Monad (t m)
                              => AppSetup a pa t fsp m )
                              -> AppSetup a pa RDONLY.ReadOnlyFilesystemSolutionT RDONLY.FileSystemSolution m
useReadOnlyFilesystemSolution s = s
    { appRunFspT = RDONLY.runReadOnlyFilesystemSolutionT
    , appUnopened = RDONLY.Unopened
    }

-- | Change a setup to use the simple persistence mechanism
useSimpleFilesystemSolution :: MonadIO m
                            => ( forall t . Monad (t m)
                            => AppSetup a pa t fsp m )
                            -> AppSetup a pa RDWR.SimpleFilesystemSolutionT RDWR.FileSystemSolution m
useSimpleFilesystemSolution s = s
    { appRunFspT = RDWR.runSimpleFilesystemSolutionT
    , appUnopened = RDWR.Unopened
    }

useCabalFilesystemSolution :: MonadIO m
                           => ( forall t . Monad (t m)
                           => AppSetup a pa t fsp m )
                           -> AppSetup a pa Cabal.CabalSolution Cabal.FileSystemSolution m
useCabalFilesystemSolution s = s
    { appRunFspT = Cabal.runCabalSolution
    , appUnopened = Cabal.Unopened
    }
-- | A setup with all fields set to bottom
blankSetup :: (Monad (t m)) => AppSetup a pa t fsp m
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
    
