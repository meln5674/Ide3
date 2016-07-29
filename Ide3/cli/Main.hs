{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImplicitParams #-}
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

import Ide3.Monad (SolutionM)
import Ide3.Mechanism.State

import Command.Trans
import Command.Types
import Command
import Viewer
import qualified ReadOnlyFilesystemSolution as RDONLY
import qualified SimpleFilesystemSolution as RDWR
import qualified CabalFilesystemSolution as Cabal

import Editor
import Builder
import Runner
import Initializer

deriving instance (MonadException m) => MonadException (SolutionStateT m)
deriving instance (MonadException m) => MonadException (StatefulSolution m)
deriving instance (MonadException m) => MonadException (RDONLY.ReadOnlyFilesystemSolutionT m)
deriving instance (MonadException m) => MonadException (Cabal.CabalSolution m)
deriving instance (MonadException m) => MonadException (RDWR.SimpleFilesystemSolutionT m)


deriving instance (MonadMask m) => MonadMask (RDONLY.ReadOnlyFilesystemSolutionT m)
deriving instance (MonadCatch m) => MonadCatch (RDONLY.ReadOnlyFilesystemSolutionT m)
deriving instance (MonadThrow m) => MonadThrow (RDONLY.ReadOnlyFilesystemSolutionT m)


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
commandList :: (MonadMask m, ViewerIOAction m u, Args a) 
            => Editor m u
            -> Builder m u
            -> Runner m u
            -> Initializer a m u
            -> [Command () (ViewerStateT m)]
commandList editor builder runner initializer =
    [ helpCmd
    , cdCmd
    , shellCmd
    , newCmd initializer
    , openCmd
    , saveAsCmd
    , saveCmd
    , modulesCmd
    , moduleCmd
    , addImportCmd
    , removeImportCmd
    , addExportCmd
    , removeExportCmd
    , declarationsCmd
    , importsCmd
    , importedCmd
    , exportsCmd
    , exportedCmd
    , visibleCmd
    , catCmd
    , addModuleCmd
    , removeModuleCmd
    , addDeclarationCmd editor
    , removeDeclarationCmd
    , editCmd editor
    , buildCmd builder
    , runCmd runner
    , treeCmd
    , searchCmd
    , quitCmd
    ]


-- | Run the main program using the specified persistance mechanism and editor
runWith :: ( MonadException (t (SolutionStateT IO))
           , MonadMask (t (SolutionStateT IO))
           , ViewerAction (t (SolutionStateT IO)) u
           , Monad (t (SolutionStateT IO))
           , Args a
           )
        => (forall b . t (SolutionStateT IO) b -> fsp -> SolutionStateT IO (b, fsp))
        -> fsp 
        -> Editor (t (SolutionStateT IO)) u
        -> Builder (t (SolutionStateT IO)) u
        -> Runner (t (SolutionStateT IO)) u
        -> Initializer a (t (SolutionStateT IO)) u
        -> IO ()
runWith runFspT unopened editor builder runner initializer = void $ 
    runViewerState runFspT unopened $
        flip runCommandT (commandList editor builder runner initializer) $ 
            runInputT settings $ do
                outputStrLn "Haskell solution viewer"
                outputStrLn "Type \"help\" for commands"
                runMain

-- | Data type which contains options for running the application
data {- (Monad (t (SolutionStateT IO))) => -} AppSetup a t fsp u
    = AppSetup
    { -- | The editor to use
      appEditor :: Editor (t (SolutionStateT IO)) u
      -- | The builder to use
    , appBuilder :: Builder (t (SolutionStateT IO)) u
      -- | The runner to use
    , appRunner :: Runner (t (SolutionStateT IO)) u
      -- | The initializer to use
    , appInitializer :: Initializer a (t (SolutionStateT IO)) u
      -- | Function for running the persistence mechanism
    , appRunFspT :: forall b . t (SolutionStateT IO) b -> fsp -> SolutionStateT IO (b, fsp)
      -- | Initial state of the persistence mechanism
    , appUnopened :: fsp
    }

-- | Run the main program with the specified setup
runWithSetup :: ( MonadException (t (SolutionStateT IO))
                , MonadMask (t (SolutionStateT IO))
                , ViewerAction (t (SolutionStateT IO)) u
                , Monad (t (SolutionStateT IO))
                , Args a
                )
             => AppSetup a t fsp u
             -> IO ()
runWithSetup setup = runWith 
    (appRunFspT setup) 
    (appUnopened setup) 
    (appEditor setup) 
    (appBuilder setup) 
    (appRunner setup) 
    (appInitializer setup)

-- | Change a setup to use the nano editor
useNanoEditor :: ( MonadIO (t (SolutionStateT IO))
                 , MonadMask (t (SolutionStateT IO))
                 , Monad (t (SolutionStateT IO))
                 ) => AppSetup a t fsp u -> AppSetup a t fsp u
useNanoEditor s = s{appEditor = nanoEditor}

useStackBuilder :: ( MonadIO (t (SolutionStateT IO))
                   , MonadMask (t (SolutionStateT IO))
                   , Monad (t (SolutionStateT IO))
                   ) => AppSetup a t fsp u -> AppSetup a t fsp u
useStackBuilder s = s{appBuilder = stackBuilder}

useStackRunner :: ( MonadIO (t (SolutionStateT IO))
                  , MonadMask (t (SolutionStateT IO))
                  , Monad (t (SolutionStateT IO))
                  ) => AppSetup a t fsp u -> AppSetup a t fsp u
useStackRunner s = s{appRunner = stackRunner}

useStackInitializer :: ( MonadIO (t (SolutionStateT IO))
                       , MonadMask (t (SolutionStateT IO))
                       , Monad (t (SolutionStateT IO))
                       , SolutionM (t (SolutionStateT IO))
                       ) => AppSetup a t fsp u -> AppSetup StackInitializerArgs t fsp u
useStackInitializer s = s{appInitializer = stackInitializer}

-- | Change a setup to use the read-only persistence mechanism
useReadOnlyFilesystemSolution :: (forall t . ( Monad (t (SolutionStateT IO))) 
                             => AppSetup a t fsp u)
                             -> AppSetup a RDONLY.ReadOnlyFilesystemSolutionT RDONLY.FileSystemSolution u
useReadOnlyFilesystemSolution s = s
    { appRunFspT = RDONLY.runReadOnlyFilesystemSolutionT
    , appUnopened = RDONLY.Unopened
    }

-- | Change a setup to use the simple persistence mechanism
useSimpleFilesystemSolution :: (forall t . Monad (t (SolutionStateT IO)) 
                           => AppSetup a t fsp u)
                           -> AppSetup a RDWR.SimpleFilesystemSolutionT RDWR.FileSystemSolution u
useSimpleFilesystemSolution s = s
    { appRunFspT = RDWR.runSimpleFilesystemSolutionT
    , appUnopened = RDWR.Unopened
    }

useCabalFilesystemSolution :: (forall t . Monad (t (SolutionStateT IO))
                          => AppSetup a t fsp u)
                          -> AppSetup a Cabal.CabalSolution Cabal.FileSystemSolution u
useCabalFilesystemSolution s = s
    { appRunFspT = Cabal.runCabalSolution
    , appUnopened = Cabal.Unopened
    }
-- | A setup with all fields set to bottom
blankSetup :: (Monad (t (SolutionStateT IO))) => AppSetup a t fsp u
blankSetup = AppSetup
           { appRunFspT = error "No file system solution defined"
           , appUnopened = error "No default solution state defined"
           , appEditor = noEditor
           , appBuilder = noBuilder
           , appRunner = noRunner
           , appInitializer = noInitializer
           }

-- | Entry point
main :: IO ()
main = runWithSetup 
     $ useStackBuilder 
     $ useStackRunner
     $ useStackInitializer
     $ useNanoEditor 
     $ useCabalFilesystemSolution 
       blankSetup
  where
    ?proxy = undefined :: Proxy ()
