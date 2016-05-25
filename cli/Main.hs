{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE NamedFieldPuns #-}
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

import Data.Proxy
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

import Ide3.Monad (ProjectM)
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
import Initializer

-- | Run a single iteration of reading input from the user, deciding which command to run,
-- running it, then printing the response, and indicating if the user wishes to quit
repl :: (MonadException m, ViewerAction m u) 
     => InputT (CommandT () (ViewerStateT m)) Bool
repl = do
    input <- getInputLine ">"
    case input of
        Nothing -> return True
        Just input -> do
            response <- lift $ execCommand input ()
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
    , newCmd initializer
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
           , ViewerAction (t (ProjectStateT IO)) u
           , Monad (t (ProjectStateT IO))
           , Args a
           )
        => (forall b . t (ProjectStateT IO) b -> fsp -> ProjectStateT IO (b, fsp))
        -> fsp 
        -> Editor (t (ProjectStateT IO)) u
        -> Builder (t (ProjectStateT IO)) u
        -> Runner (t (ProjectStateT IO)) u
        -> Initializer a (t (ProjectStateT IO)) u
        -> IO ()
runWith runFspT unopened editor builder runner initializer = void $ 
    runViewerState runFspT unopened $
        flip runCommandT (commandList editor builder runner initializer) $ 
            runInputT settings $ do
                outputStrLn "Haskell project viewer"
                outputStrLn "Type \"help\" for commands"
                runMain

-- | Data type which contains options for running the application
data (Monad (t (ProjectStateT IO))) => AppSetup a t fsp u
    = AppSetup
    { -- | The editor to use
      appEditor :: Editor (t (ProjectStateT IO)) u
      -- | The builder to use
    , appBuilder :: Builder (t (ProjectStateT IO)) u
      -- | The runner to use
    , appRunner :: Runner (t (ProjectStateT IO)) u
      -- | The initializer to use
    , appInitializer :: Initializer a (t (ProjectStateT IO)) u
      -- | Function for running the persistence mechanism
    , appRunFspT :: forall b . t (ProjectStateT IO) b -> fsp -> ProjectStateT IO (b, fsp)
      -- | Initial state of the persistence mechanism
    , appUnopened :: fsp
    }

-- | Run the main program with the specified setup
runWithSetup :: ( MonadException (t (ProjectStateT IO))
                , MonadMask (t (ProjectStateT IO))
                , ViewerAction (t (ProjectStateT IO)) u
                , Monad (t (ProjectStateT IO))
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
useNanoEditor :: ( MonadIO (t (ProjectStateT IO))
                 , MonadMask (t (ProjectStateT IO))
                 , Monad (t (ProjectStateT IO))
                 ) => AppSetup a t fsp u -> AppSetup a t fsp u
useNanoEditor s = s{appEditor = nanoEditor}

useStackBuilder :: ( MonadIO (t (ProjectStateT IO))
                   , MonadMask (t (ProjectStateT IO))
                   , Monad (t (ProjectStateT IO))
                   ) => AppSetup a t fsp u -> AppSetup a t fsp u
useStackBuilder s = s{appBuilder = stackBuilder}

useStackRunner :: ( MonadIO (t (ProjectStateT IO))
                  , MonadMask (t (ProjectStateT IO))
                  , Monad (t (ProjectStateT IO))
                  ) => AppSetup a t fsp u -> AppSetup a t fsp u
useStackRunner s = s{appRunner = stackRunner}

useStackInitializer :: ( MonadIO (t (ProjectStateT IO))
                       , MonadMask (t (ProjectStateT IO))
                       , Monad (t (ProjectStateT IO))
                       , ProjectM (t (ProjectStateT IO))
                       ) => AppSetup a t fsp u -> AppSetup StackInitializerArgs t fsp u
useStackInitializer s = s{appInitializer = stackInitializer}

-- | Change a setup to use the read-only persistence mechanism
useReadOnlyFilesystemProject :: (forall t . ( Monad (t (ProjectStateT IO))) 
                             => AppSetup a t fsp u)
                             -> AppSetup a RDONLY.ReadOnlyFilesystemProjectT RDONLY.FileSystemProject u
useReadOnlyFilesystemProject s = s
    { appRunFspT = RDONLY.runReadOnlyFilesystemProjectT
    , appUnopened = RDONLY.Unopened
    }

-- | Change a setup to use the simple persistence mechanism
useSimpleFilesystemProject :: (forall t . Monad (t (ProjectStateT IO)) 
                           => AppSetup a t fsp u)
                           -> AppSetup a RDWR.SimpleFilesystemProjectT RDWR.FileSystemProject u
useSimpleFilesystemProject s = s
    { appRunFspT = RDWR.runSimpleFilesystemProjectT
    , appUnopened = RDWR.Unopened
    }

-- | A setup with all fields set to bottom
blankSetup :: (Monad (t (ProjectStateT IO))) => AppSetup a t fsp u
blankSetup = AppSetup
           { appRunFspT = error "No file system project defined"
           , appUnopened = error "No default project state defined"
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
     $ useSimpleFilesystemProject 
       blankSetup
  where
    ?proxy = undefined :: Proxy ()
