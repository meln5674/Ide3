module Main where

import Control.Monad.Trans
import Control.Monad.Trans.Except
import Control.Monad.Trans.State.Strict
import Control.Monad

import System.Environment
import System.IO (hFlush, stdout)

import Ide3.Monad

import qualified Ide3.Declaration as Declaration
import qualified Ide3.Module as Module

import Ide3.Types (items, Module, ModuleInfo(..), Symbol(..), getChild, ProjectError)

import Ide3.Mechanism.State ( runProjectStateT )

import qualified CmdParser as Cmd
import CmdParser
import Digest
import Viewer

readCmd :: IO (Maybe Cmd)
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
        , "imported: list the symbols imported by the current module"
        , "exports: list the exports in the current module"
        , "exported: list the symbols exported by the current module"
        , "quit: exit the program"
        ]

printOnError f = do
    runExceptT $ catchE f $ liftIO . putStrLn
    return ()

withSelectedModule :: Show a => (Module -> ExceptT ProjectError ViewerStateM [a]) -> ViewerStateM ()
withSelectedModule f = printOnError $ do
    name <- lift $ gets currentModule
    case name of
        Nothing -> throwE "No module currently selected"
        Just name -> do
            mod <- getModule (ModuleInfo (Symbol name))
            xs <- f mod
            liftIO $ mapM_ print xs

doHelp :: ViewerStateM ()
doHelp = liftIO printHelp

doOpen :: FilePath -> ViewerStateM ()
doOpen path = printOnError $ do
    lift . lift $ put (ToOpen path)
    lift $ modify $ \s -> s{currentModule=Nothing}
    load

doModules = printOnError $ do
    names <- getModules
    liftIO $ mapM_ print names

doModule :: String -> ViewerStateM ()
doModule name = printOnError $ do
    getModule (ModuleInfo (Symbol name))
    lift $ modify $ \s -> s{currentModule=Just name}

doDeclarations :: ViewerStateM ()
doDeclarations = withSelectedModule 
    $ return 
    . map (Declaration.info) 
    . items 
    . Module.getDeclarations

doImports :: ViewerStateM ()
doImports = withSelectedModule 
    $ return
    . items
    . Module.getImports

doImported :: ViewerStateM ()
doImported = withSelectedModule
    Module.importedSymbols
    
doExports :: ViewerStateM ()
doExports = withSelectedModule
    $ return
    . items
    . Module.getExports

doExported :: ViewerStateM ()
doExported = withSelectedModule
    $   (return 
    .   map getChild)
    <=< Module.exportedSymbols 

-- (a -> m b) -> (b -> c) -> c

doQuit :: ViewerStateM ()
doQuit = return ()



repl = do
    input <- liftIO $ readCmd
    case input of
        Just Help -> doHelp >> return True
        Just (Open path) -> doOpen path >> return True
        Just Modules -> doModules >> return True
        Just (Module name) -> doModule name >> return True
        Just Declarations -> doDeclarations >> return True
        Just Imports -> doImports >> return True
        Just Imported -> doImported >> return True
        Just Exports -> doExports >> return True
        Just Exported -> doExported >> return True
        Just Quit -> return False
        Nothing -> return True

runMain = do
    liftIO $ putStr ">" >> hFlush stdout
    continue <- repl
    when continue runMain
      
main :: IO ()
main = do
    putStrLn "Haskell project viewer"
    putStrLn "Type \"help\" for commands"
    runViewerState runMain
    return ()
