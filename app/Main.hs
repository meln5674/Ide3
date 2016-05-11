{-# LANGUAGE RankNTypes #-}
{-|
Module      : Main
Description : Demo project main module
Copyright   : (c) Andrew Melnick, 2016

License     : BSD3
Maintainer  : meln5674@kettering.edu
Stability   : experimental
Portability : POSIX

This is the main module for the demo haskell IDE project
-}
module Main where

import Data.List
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map

import Control.Monad.Catch

import Control.Monad.Trans
import Control.Monad.Trans.Except
import Control.Monad.Trans.State.Strict
import Control.Monad

import System.Console.Haskeline

import Ide3.Mechanism.State (ProjectStateT)

import qualified CmdParser as Cmd
import CmdParser
import Command.Trans
import Command.Types
import Command
import Viewer
import qualified ReadOnlyFilesystemProject as RDONLY
import qualified SimpleFilesystemProject as RDWR
import Editor
import Builder
import Runner

-- | Run a single iteration of reading input from the user, deciding which command to run,
-- running it, then printing the response, and indicating if the user wishes to quit
repl :: (MonadException m, ViewerMonad m) 
     => InputT (CommandT UserError (ViewerStateT m)) Bool
repl = do
    input <- getInputLine ">"
    case input of
        Nothing -> return True
        Just input -> do
            response <- lift $ execCommand input ()
            outputStrLn response
            continue <- lift $ getExitFlag
            return continue

-- | Run the main program loop forever until the user indicates they wish to quit
runMain :: (MonadException m, ViewerMonad m) 
        => InputT (CommandT UserError (ViewerStateT m)) ()
runMain = do
    continue <- repl
    when continue runMain

-- | Settings for the line editor transformer
settings :: (MonadException m, ViewerMonad m) 
         => Settings (CommandT UserError (ViewerStateT m))
settings = Settings{complete=cmdCompletion, historyFile=Nothing, autoAddHistory=True}

-- | List of commands, using a specified editor
commandList :: (MonadMask m, MonadIO m, ViewerMonad m) 
            => (forall u . Editor m u) 
            -> (forall u . Builder m u)
            -> (forall u . Runner m u)
            -> [Command u (ViewerStateT m)]
commandList editor builder runner =
    [ helpCmd
    , newCmd
    , openCmd
    , saveAsCmd
    , saveCmd
    , modulesCmd
    , moduleCmd
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
runWith :: ( MonadException (t (ProjectStateT IO))
           , MonadMask (t (ProjectStateT IO))
           , ViewerMonad (t (ProjectStateT IO))
           , Monad (t (ProjectStateT IO))
           )
        => (forall b . t (ProjectStateT IO) b -> fsp -> ProjectStateT IO (b, fsp))
        -> fsp 
        -> (forall u . Editor (t (ProjectStateT IO)) u)
        -> (forall u . Builder (t (ProjectStateT IO)) u)
        -> (forall u . Runner (t (ProjectStateT IO)) u)
        -> IO ()
runWith runFspT unopened editor builder runner = void $ 
    runViewerState runFspT unopened $
        flip runCommandT (commandList editor builder runner) $ 
            runInputT settings $ do
                outputStrLn "Haskell project viewer"
                outputStrLn "Type \"help\" for commands"
                runMain

-- | Data type which contains options for running the application
data (Monad (t (ProjectStateT IO))) => AppSetup t fsp u
    = AppSetup
    { -- | The editor to use
      appEditor :: Editor (t (ProjectStateT IO)) u
      -- | The builder to use
    , appBuilder :: Builder (t (ProjectStateT IO)) u
      -- | The runner to use
    , appRunner :: Runner (t (ProjectStateT IO)) u
      -- | Function for running the persistence mechanism
    , appRunFspT :: forall b . t (ProjectStateT IO) b -> fsp -> ProjectStateT IO (b, fsp)
      -- | Initial state of the persistence mechanism
    , appUnopened :: fsp
    }

-- | Run the main program with the specified setup
runWithSetup :: ( MonadException (t (ProjectStateT IO))
                , MonadMask (t (ProjectStateT IO))
                , ViewerMonad (t (ProjectStateT IO))
                , Monad (t (ProjectStateT IO))
                )
             => (forall u . AppSetup t fsp u)
             -> IO ()
runWithSetup setup = runWith (appRunFspT setup) (appUnopened setup) (appEditor setup) (appBuilder setup) (appRunner setup)

-- | Change a setup to use the nano editor
useNanoEditor :: ( MonadIO (t (ProjectStateT IO))
                 , MonadMask (t (ProjectStateT IO))
                 , Monad (t (ProjectStateT IO))
                 ) => AppSetup t fsp u -> AppSetup t fsp u
useNanoEditor = \s -> s{appEditor = nanoEditor}

useStackBuilder :: ( MonadIO (t (ProjectStateT IO))
                   , MonadMask (t (ProjectStateT IO))
                   , Monad (t (ProjectStateT IO))
                   ) => AppSetup t fsp u -> AppSetup t fsp u
useStackBuilder = \s -> s{appBuilder = stackBuilder}

useStackRunner :: ( MonadIO (t (ProjectStateT IO))
                  , MonadMask (t (ProjectStateT IO))
                  , Monad (t (ProjectStateT IO))
                  ) => AppSetup t fsp u -> AppSetup t fsp u
useStackRunner = \s -> s{appRunner = stackRunner}

-- | Change a setup to use the read-only persistence mechanism
useReadOnlyFilesystemProject :: (forall t . ( Monad (t (ProjectStateT IO))) 
                             => AppSetup t fsp u)
                             -> AppSetup RDONLY.ReadOnlyFilesystemProjectT RDONLY.FileSystemProject u
useReadOnlyFilesystemProject s = s
    { appRunFspT = RDONLY.runReadOnlyFilesystemProjectT
    , appUnopened = RDONLY.Unopened
    }

-- | Change a setup to use the simple persistence mechanism
useSimpleFilesystemProject :: (forall t . Monad (t (ProjectStateT IO)) 
                           => AppSetup t fsp u)
                           -> AppSetup RDWR.SimpleFilesystemProjectT RDWR.FileSystemProject u
useSimpleFilesystemProject s = s
    { appRunFspT = RDWR.runSimpleFilesystemProjectT
    , appUnopened = RDWR.Unopened
    }

-- | A setup with all fields set to bottom
blankSetup :: (Monad (t (ProjectStateT IO))) => AppSetup t fsp u
blankSetup = AppSetup
           { appRunFspT = error "No file system project defined"
           , appUnopened = error "No default project state defined"
           , appEditor = noEditor
           , appBuilder = noBuilder
           , appRunner = noRunner
           }

-- | Entry point
main :: IO ()
main = runWithSetup 
     $ useStackBuilder 
     $ useStackRunner
     $ useNanoEditor 
     $ useSimpleFilesystemProject 
     $ blankSetup
