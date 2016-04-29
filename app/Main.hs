{-# LANGUAGE RankNTypes #-}
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
import Command
import Viewer
import qualified ReadOnlyFilesystemProject as RDONLY
import qualified SimpleFilesystemProject as RDWR
import Editor

-- | Run a single iteration of reading input from the user, deciding which command to run,
-- running it, then printing the response, and indicating if the user wishes to quit
repl :: (MonadException m, ViewerMonad m) => InputT (CommandT UserError (ViewerStateT m)) Bool
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
runMain :: (MonadException m, ViewerMonad m) => InputT (CommandT UserError (ViewerStateT m)) ()
runMain = do
    continue <- repl
    when continue runMain

-- | Settings for the line editor transformer
settings :: (MonadException m, ViewerMonad m) => Settings (CommandT UserError (ViewerStateT m))
settings = Settings{complete=cmdCompletion, historyFile=Nothing, autoAddHistory=True}

-- | List of commands
commandList :: (MonadMask m, MonadIO m, ViewerMonad m) => (forall u . Editor m u) -> [Command u (ViewerStateT m)]
commandList editor =
    [ helpCmd
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
    , treeCmd
    , searchCmd
    , quitCmd
    ]


-- | Run the main program using the specified persistance mechanism
runWith :: ( MonadException (t (ProjectStateT IO))
           , MonadMask (t (ProjectStateT IO))
           , ViewerMonad (t (ProjectStateT IO))
           )
        => (forall b . t (ProjectStateT IO) b -> fsp -> ProjectStateT IO (b, fsp))
        -> fsp 
        -> (forall u . Editor (t (ProjectStateT IO)) u)
        -> IO ()
runWith runFspT unopened editor = void $ 
    runViewerState runFspT unopened $
        flip runCommandT (commandList editor) $ 
            runInputT settings $ do
                outputStrLn "Haskell project viewer"
                outputStrLn "Type \"help\" for commands"
                runMain

{-
-- | Run the program with the read-only persistance mechanism
runWithReadOnlyFilesystemProject :: (forall u . Editor 
                                        (RDONLY.ReadOnlyFilesystemProjectT 
                                            (ProjectStateT IO)
                                        ) u
                                    ) -> IO ()
runWithReadOnlyFilesystemProject editor = runWith RDONLY.runReadOnlyFilesystemProjectT RDONLY.Unopened editor

-- | Run the program with the simple persistance mechanism
runWithSimpleFilesystemProject :: (forall u . Editor 
                                    (RDWR.SimpleFilesystemProjectT 
                                            (ProjectStateT IO)
                                    ) u
                                  ) -> IO ()
runWithSimpleFilesystemProject editor = runWith RDWR.runSimpleFilesystemProjectT RDWR.Unopened editor
-}

data AppSetup t fsp u
    = AppSetup
    { appEditor :: Editor (t (ProjectStateT IO)) u
    , appRunFspT :: forall b . t (ProjectStateT IO) b -> fsp -> ProjectStateT IO (b, fsp)
    , appUnopened :: fsp
    }

runWithSetup :: ( MonadException (t (ProjectStateT IO))
                , MonadMask (t (ProjectStateT IO))
                , ViewerMonad (t (ProjectStateT IO))
                )
             => (forall u . AppSetup t fsp u)
             -> IO ()
runWithSetup setup = runWith (appRunFspT setup) (appUnopened setup) (appEditor setup)

useNanoEditor :: ( MonadIO (t (ProjectStateT IO))
                 , MonadMask (t (ProjectStateT IO))
                 ) => AppSetup t fsp u -> AppSetup t fsp u
useNanoEditor = \s -> s{appEditor = nanoEditor}

useReadOnlyFilesystemProject :: (forall t . AppSetup t fsp u) -> AppSetup RDONLY.ReadOnlyFilesystemProjectT RDONLY.FileSystemProject u
useReadOnlyFilesystemProject s = s
    { appRunFspT = RDONLY.runReadOnlyFilesystemProjectT
    , appUnopened = RDONLY.Unopened
    }

useSimpleFilesystemProject :: (forall t . AppSetup t fsp u) -> AppSetup RDWR.SimpleFilesystemProjectT RDWR.FileSystemProject u
useSimpleFilesystemProject s = s
    { appRunFspT = RDWR.runSimpleFilesystemProjectT
    , appUnopened = RDWR.Unopened
    }

blankSetup :: AppSetup t fsp u
blankSetup = AppSetup{appRunFspT = undefined, appUnopened = undefined, appEditor = undefined}

main :: IO ()
main = runWithSetup $ useNanoEditor $ useSimpleFilesystemProject $ blankSetup
