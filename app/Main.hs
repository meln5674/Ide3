module Main where

import Control.Monad.Trans
import Control.Monad.Trans.Except
import Control.Monad.Trans.State

import System.Environment

import Ide3.Monad

import qualified Ide3.Declaration as Declaration
import Ide3.Module (getDeclarations, getImports, getExports)

import Ide3.Types (items, ModuleInfo(..), Symbol(..))
import Ide3.HasA

import Ide3.Mechanism.State ( runProjectStateT )

import qualified CmdParser as Cmd
import CmdParser
import Digest
import Viewer

{-
run :: FilePath -> IO ()
run path = do
    runProjectStateT $ runExceptT $ do
        let f = do
                digestProject path
                moduleList <- getModules
                liftIO $ print (length moduleList)
        catchE f (liftIO . putStrLn)
    return ()
-}

readCmd = do
    input <- getLine
    case Cmd.parse input of
        Right cmd -> return $ Just cmd
        Left err -> putStrLn err >> return Nothing

printHelp = do
    mapM_ putStrLn $
        [ "Commands: "
        , ""
        , "help: show this message"
        , "open PATH: open a project rooted at PATH"
        , "modules: show a list of modules in the current project"
        , "module MODULE: set MODULE as the current module"
        , "declarations: list the declarations in the current module"
        , "imports: list the imports in the current module"
        , "exports: list the exports in the current module"
        , "quit: exit the program"
        ]

printOnError f = do
    runExceptT $ catchE f $ liftIO . putStrLn
    return ()

doHelp :: ViewerStateM ()
doHelp = liftIO printHelp

doOpen :: FilePath -> ViewerStateM ()
doOpen path = printOnError $ do
    lift . lift $ put (ToOpen path)
    load
doModules = printOnError $ do
    names <- getModules
    liftIO $ mapM_ print names

doModule :: String -> ViewerStateM ()
doModule name = printOnError $ do
    getModule (ModuleInfo (Symbol name))
    lift $ modify $ \s -> s{currentModule=Just name}

doDeclarations = printOnError $ do
    name <- lift $ gets currentModule
    case name of
        Nothing -> throwE "No module currently selected"
        Just name -> do
            mod <- getModule (ModuleInfo (Symbol name))
            let infos = map (Declaration.info) $ items $ getDeclarations mod
            liftIO $ mapM_ print infos
doImports = printOnError $ do
    name <- lift $ gets currentModule
    case name of
        Nothing -> throwE "No module currently selected"
        Just name -> do
            mod <- getModule (ModuleInfo (Symbol name))
            let infos = items $ getImports mod
            liftIO $ mapM_ print infos
doExports = printOnError $ do
    name <- lift $ gets currentModule
    case name of
        Nothing -> throwE "No module currently selected"
        Just name -> do
            mod <- getModule (ModuleInfo (Symbol name))
            let infos = items $ getExports mod
            liftIO $ mapM_ print infos

doQuit :: ViewerStateM ()
doQuit = return ()

runMain = do
    input <- liftIO $ readCmd
    case input of
        Just Help -> doHelp >> runMain
        Just (Open path) -> doOpen path >> runMain
        Just Modules -> doModules >> runMain
        Just (Module name) -> doModule name >> runMain
        Just Declarations -> doDeclarations >> runMain
        Just Imports -> doImports >> runMain
        Just Exports -> doExports >> runMain
        Just Quit -> return ()
        Nothing -> runMain
        
main :: IO ()
main = runViewerState runMain
{-do
    args <- getArgs
    case args of
        [path] -> run path
        _ -> putStrLn "Expecting 1 and only 1 path"
-}
