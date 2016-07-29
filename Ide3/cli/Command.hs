{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ConstraintKinds #-}
{-|
Module      : Command
Description : Commands for the demo project
Copyright   : (c) Andrew Melnick, 2016

License     : BSD3
Maintainer  : meln5674@kettering.edu
Stability   : experimental
Portability : POSIX

This module defines a monad transformer, CommandT, which represents actions
which take place using a set of textual commands

This module also provides a data type which represents said commands, and a set
of default commands.
-}
module Command where

import Data.Proxy

import Data.Maybe
import Data.List

import qualified Data.Map as Map

import System.Process
import System.Directory

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Except
import Control.Monad.Trans.Writer
import Control.Monad.Trans.State.Strict

import System.Console.Haskeline

import Ide3.Mechanism
import Ide3.Monad


import qualified Ide3.Declaration as Declaration
import qualified Ide3.Module as Module
import qualified Ide3.Module.Query as Module

import Ide3.Utils
import Ide3.Types
    ( item
    , items
    , body
    , ProjectInfo(..)
    , Module
    , ModuleInfo(..)
    , Symbol(..)
    , getChild
    , DeclarationInfo (..)
    , SolutionError (..)
    , ModuleChild (..)
    , ProjectChild (..)
    , Qualify (..)
    , SolutionResult
    )

import Ide3.Mechanism.State()
import Ide3.ModuleTree


import Command.Types
import Command.Trans

import CmdParser
import Viewer
import Editor
import Builder
import Runner
import Initializer

type ViewerAction m u = (?proxy :: Proxy u, ViewerMonad m, Show u)
type ViewerIOAction m u = (?proxy :: Proxy u, ViewerMonad m, Show u, MonadIO m)

-- | Run a project command, and return either the message or error it produced
printOnError :: (ViewerAction m u)
             => SolutionResult (ViewerStateT m) u String 
             -> ViewerStateT m String
printOnError f = liftM (either show id) $ runExceptT f

withSelectedProject :: (Show a, ViewerAction m u)
                    => (ProjectInfo -> SolutionResult (ViewerStateT m) u [Output a])
                    -> ViewerStateT m String
withSelectedProject f = printOnError $ do
    maybeProjectInfo <- lift $ gets currentProject
    case maybeProjectInfo of
        Nothing -> throwE $ InvalidOperation "No project currently selected" ""
        Just pi -> do
            xs <- f pi
            return $ intercalate "\n" $ map show xs

-- | Perform some action which requires the current module open in the viewer
withSelectedModule :: (Show a, ViewerAction m u) 
                   => (ProjectInfo -> Module -> SolutionResult (ViewerStateT m) u [Output a])
                   -> ViewerStateT m String
withSelectedModule f = withSelectedProject $ \pi -> do
    maybeModuleInfo <- lift $ gets currentModule
    case maybeModuleInfo of
        Nothing -> throwE $ InvalidOperation "No module currently selected" ""
        Just mi -> do
            module_ <- getModule pi mi
            f pi module_
            
-- | Action for the help command
doHelp :: ViewerAction m u => CommandT u' m String
doHelp = printHelp

doShell :: ViewerIOAction m u => String -> ViewerStateT m String
doShell cmd = printOnError $ do
    let p = shell cmd
    (_, out, err) <- liftIO $ readCreateProcessWithExitCode p ""
    return $ out ++ err

doCd :: ViewerIOAction m u => FilePath -> ViewerStateT m String
doCd path = printOnError $ do
    liftIO $ setCurrentDirectory path
    return ""

doNew :: (ViewerAction m u, Args a) => Initializer a m u -> String -> ViewerStateT m String
doNew initializer arg = printOnError $ do
    case runInitializerWithInput initializer =<< (parseArityN arg) of
        Left msg -> return msg
        Right initializerAction -> do
            r <- ExceptT $ lift $ runExceptT $ initializerAction
            case r of
                InitializerSucceeded out err -> return $ out ++ err
                InitializerFailed out err -> return $ out ++ err

-- | Action for the open command
doOpen :: forall m u . (ViewerIOAction m u) => FilePath -> ViewerStateT m String
doOpen path = printOnError $ do
    openSolution path -- 1:: SolutionResult (ViewerStateT m 
    return "Loaded"

-- | Action for the save command
doSave :: (ViewerIOAction m u) => ViewerStateT m String
doSave = printOnError $ do
    saveSolution Nothing
    return "Saved"

-- | Action for the save as command
doSaveAs :: (ViewerIOAction m u) => FilePath -> ViewerStateT m String
doSaveAs p = printOnError $ do
    saveSolution $ Just p
    return "Saved"

-- | Action for the modules command
doModules :: ViewerAction m u => ViewerStateT m String
doModules = withSelectedProject $ \pi -> asShows <$> getModules pi

-- | Action for the module command
doModule :: ViewerAction m u => String -> ViewerStateT m String
doModule name = withSelectedProject $ \pi -> do
    let info = (ModuleInfo (Symbol name))
    _ <- getModule pi info
    lift $ modify $ \s -> s{currentModule=Just info}
    return $ defaultOutputs []

-- | Action for the declarations command
doDeclarations :: ViewerAction m u => ViewerStateT m String
doDeclarations = withSelectedModule $ \_ m -> 
      return 
    $ asShows
    $ map Declaration.info
    $ items 
    $ Module.getDeclarations m

-- | Action for the imports command
doImports :: ViewerAction m u => ViewerStateT m String
doImports = withSelectedModule $ \pi mi -> 
      return
    $ asShows
    $ items
    $ Module.getImports mi

-- | Action for the imported command
doImported :: ViewerAction m u => ViewerStateT m String
doImported = withSelectedModule $ \pi m -> 
      liftM asShows 
    $ Module.importedSymbols pi m

-- | Action for the exports command
doExports :: ViewerAction m u => ViewerStateT m String
doExports = withSelectedModule $ \pi m -> return $ case Module.getExports m of
    Nothing -> [asString "Exports everything"]
    Just exports -> asShows
                  $ items
                  $ (map snd . Map.toList)
                    exports
                   

-- | Action for the exported command
doExported :: ViewerAction m u => ViewerStateT m String
doExported = withSelectedModule $ \pi mi -> 
      liftM (asShows . map getChild)
    $ Module.exportedSymbols pi mi

-- | Action for the visible command
doVisible :: ViewerAction m u => ViewerStateT m String
doVisible = withSelectedModule $ \pi m -> liftM asShows $ Module.internalSymbols pi m

-- | Action for the cat command
doCat :: ViewerAction m u => String -> ViewerStateT m String
doCat sym = withSelectedModule $ \pi module_ -> do
    decl <- Module.getDeclaration (DeclarationInfo (Symbol sym)) module_
    return $ defaultOutputs . asStrings . lines . body $ decl

-- | Action for the add module command
doAddModule :: ViewerAction m u => String -> ViewerStateT m String
doAddModule moduleName = withSelectedProject $ \pi -> do
    createModule pi (ModuleInfo (Symbol moduleName))
    return $ defaultOutputs [asString "Added"]

-- | Action for the remove module command
doRemoveModule :: ViewerAction m u => String -> ViewerStateT m String
doRemoveModule moduleName = withSelectedProject $ \pi -> do
    removeModule pi (ModuleInfo (Symbol moduleName))
    return $ defaultOutputs $ [asString "Removed"]

doAddExport :: ViewerAction m u => String -> ViewerStateT m String
doAddExport export = withSelectedModule $ \pi module_ -> do
    _ <- addRawExport pi (Module.info module_) export
    return [asString "Added" :: Output Bool]

doRemoveExport :: ViewerAction m u => String -> ViewerStateT m String
doRemoveExport = undefined

doAddImport :: ViewerAction m u => String -> ViewerStateT m String
doAddImport import_ = withSelectedModule $ \pi module_ -> do
    _ <- addRawImport pi (Module.info module_) $ "import " ++ import_
    return [asString "Added" :: Output Bool]

doRemoveImport :: ViewerAction m u => String -> ViewerStateT m String
doRemoveImport = undefined

-- | Action for the add decl command
doAddDeclaration :: (ViewerIOAction m u) => Editor m u -> ViewerStateT m String
doAddDeclaration editor = withSelectedModule $ \pi module_ -> do
    newDecl <- ExceptT $ lift $ runExceptT $ runEditor editor ""
    case newDecl of
        EditConfirmed newBody -> do
            toAdd <- ExceptT $ return $ Declaration.parseAndCombine newBody Nothing
            addDeclaration pi (Module.info module_) toAdd
            return [asString "Added"]
        DeleteConfirmed -> error "Deleted a new declaration?"
        EditCanceled -> return [asString "Add canceled" :: Output Bool]

-- | Action for the remove decl command
doRemoveDeclaration :: ViewerAction m u => String -> ViewerStateT m String
doRemoveDeclaration sym = withSelectedModule $ \pi module_ -> do
    removeDeclaration pi (Module.info module_) (DeclarationInfo (Symbol sym))
    return [asString "Removed" :: Output Bool]

-- | Action for the edit command
doEdit :: forall m u . (ViewerIOAction m u) => Editor m u -> String -> ViewerStateT m String
doEdit editor sym = withSelectedModule $ \pi module_ -> do
    let declInfo = DeclarationInfo (Symbol sym)
    let moduleInfo = Module.info module_
    decl <- Module.getDeclaration declInfo module_
    let declBody = body decl
    newDeclBody <- ExceptT $ lift $ runExceptT $ runEditor editor declBody
    case newDeclBody of
        EditConfirmed newBody -> do 
            let r = Declaration.parseAndCombineLenient newBody Nothing declInfo
            case r of
                Right decl -> do
                    editDeclaration pi moduleInfo declInfo $ \_ -> Right decl
                    return [asShow "Edit completed"]
                Left (decl,err) -> do
                    editDeclaration pi moduleInfo declInfo $ \_ -> Right decl
                    return [asShow $ show (err :: SolutionError u)]
        DeleteConfirmed -> do
            removeDeclaration pi moduleInfo declInfo
            return [asShow "Delete completed"]
        EditCanceled -> return [asShow "Edit canceled"]

doBuild :: (ViewerIOAction m u) => Builder m u -> ViewerStateT m String
doBuild builder = printOnError $ do
    prepareBuild
    r <- ExceptT $ lift $ runExceptT $ runBuilder builder
    case r of
        BuildSucceeded out err -> return $ out ++ err
        BuildFailed out err -> return $ out ++ err

doRun :: (ViewerIOAction m u) => Runner m u -> ViewerStateT m String
doRun runner = printOnError $ do
    r <- ExceptT $ lift $ runExceptT $ runRunner runner
    case r of
        RunSucceeded out err -> return $ out ++ err
        RunFailed out err -> return $ out ++ err
    
-- | Action for the tree command
doTree :: ViewerAction m u => ViewerStateT m String
doTree = withSelectedProject $ \pi -> do
    trees <- makeTree pi
    return $ defaultOutputs $ map (asString . formatTree) trees
    --return $ show trees

-- | Action for the search command
doSearch :: ViewerAction m u => String -> ViewerStateT m String
doSearch sym = printOnError $ do
    projects <- getProjects
    modules <- liftM concat
        $ forM projects $ \pi -> liftM (map (\mi -> (pi,mi))) $ getModules pi
    let matchingDeclsFrom pi mi = do
            decls <- lift $ getDeclarations pi mi
            let matches = filter (\(DeclarationInfo (Symbol sym')) -> sym `isInfixOf` sym') decls
                taggedMatches = map (ProjectChild pi . ModuleChild mi) matches
            tell taggedMatches
    matchingDecls <- execWriterT $ forM modules $ uncurry matchingDeclsFrom
    return $ intercalate "\n" $ map (show . fmap qual) matchingDecls

-- | Action for the quit command
doQuit :: ViewerAction m u => CommandT u' (ViewerStateT m) String
doQuit = do
    setExitFlag
    return "Exiting..."


fileNameCompletion :: ViewerIOAction m u => String -> (ViewerStateT m) (Maybe [Completion])
fileNameCompletion s = do
    comps <- listFiles s
    return $ Just $ flip map comps $ \comp -> comp{replacement = drop (length s) $ replacement comp }

-- | Given a prefix, find module names that have that prefix
moduleNameCompletion :: ViewerAction m u => String -> (ViewerStateT m) (Maybe [Completion])
moduleNameCompletion s = do
    p <- hasOpenedSolution
    if not p
        then return Nothing
        else do
            maybepi <- gets currentProject
            case maybepi of
                Nothing -> return Nothing
                Just pi -> do
                    r <- runExceptT $ do
                        mods <- getModules pi
                        let modNames = catMaybes $ for mods  $ \case
                                (ModuleInfo (Symbol sym)) -> Just sym
                                _ -> Nothing
                        let matchingNames = filter (s `isPrefixOf`) modNames
                        return $ Just $ for matchingNames $ \n ->
                            Completion{ replacement = drop (length s) n
                                      , display = n
                                      , isFinished = not $ any (\n' -> n `isPrefixOf` n' && n /= n') matchingNames
                                      }
                    return $ case r of
                        Right x -> x
                        Left _ -> Nothing

-- | Given a prefix, find declarations in the current module which have that
-- prefix
declarationNameCompletion :: ViewerAction m u => String -> (ViewerStateT m) (Maybe [Completion])
declarationNameCompletion s = do
    maybepi <- gets currentProject
    mayben <- gets currentModule
    case (maybepi, mayben) of
        (Just pi, Just info@(ModuleInfo (Symbol n))) -> do
            r <- runExceptT $ do
                m <- getModule pi info
                let infos = map (Declaration.info . item) $ Module.getDeclarations m
                let syms = for infos $ \(DeclarationInfo (Symbol sym)) -> sym
                let matchingSyms = filter (s `isPrefixOf`) syms
                return $ Just $ for matchingSyms $ \sym -> 
                    Completion{ replacement = drop (length s) sym
                              , display = sym
                              , isFinished = not $ any (\sym' -> sym `isPrefixOf` sym' && sym /= sym') matchingSyms
                              }    
            return $ case r of
                Right x -> x
                Left _ -> Nothing
        _ -> return Nothing




-- | The help command, prints the help lines for all other commands
helpCmd :: ViewerAction m u => Command u' (ViewerStateT m) 
helpCmd = Command
    { helpLine = "help: show this message"
    , root = "help"
    , parser = parseArity0 "help"
    , isAllowed = return True
    , completion = \_ -> return $ Just []
    , action = \_ -> doHelp
    }

shellCmd :: ViewerIOAction m u => Command u' (ViewerStateT m)
shellCmd = Command
    { helpLine = "shell CMD: execute a command in the shell"
    , root = "shell"
    , parser = parseArity1 "shell" "command"
    , isAllowed = return True
    , completion = \_ -> return $ Just []
    , action = liftCmd . doShell
    }

cdCmd :: ViewerIOAction m u => Command u' (ViewerStateT m)
cdCmd = Command
    { helpLine = "cd PATH: Change the current directory"
    , root = "cd"
    , parser = parseArity1 "cd" "path"
    , isAllowed = return True
    , completion = fileNameCompletion
    , action = liftCmd . doCd
    }

newCmd :: (ViewerIOAction m u, Args a, Show u) => Initializer a m u -> Command u' (ViewerStateT m)
newCmd initializer = Command
    { helpLine = "new ARGS: create a new project"
    , root = "new"
    , parser = parseArity1 "new" "argument"
    , isAllowed = return True
    , completion = \_ ->    return $ Just []
    , action = liftCmd . doNew initializer
    }

-- | The open command, takes a path and opens the project at that file or 
-- directory
openCmd :: (ViewerIOAction m u) => Command u'(ViewerStateT m) 
openCmd = Command
    { helpLine = "open PATH: open a project rooted at PATH"
    , root = "open"
    , parser = parseArity1 "open" "directory path"
    , isAllowed = return True
    , completion =  fileNameCompletion
    , action = liftCmd . doOpen
    }

-- | The save command, saves the project at whatever path was last used to save
saveCmd :: (ViewerIOAction m u) => Command u'(ViewerStateT m)  
saveCmd = Command
    { helpLine = "save: save the project at the current path"
    , root = "save"
    , parser = parseArity0 "save"
    , isAllowed = hasOpenedSolution
    , completion = fileNameCompletion
    , action = \_ -> liftCmd doSave
    }

-- | The save as command, saves the project at the specified path
saveAsCmd :: (ViewerIOAction m u) => Command u'(ViewerStateT m) 
saveAsCmd = Command
    { helpLine = "save as PATH: save the project at PATH"
    , root = "save as"
    , parser = parseArity1 "save as" "path"
    , isAllowed = hasOpenedSolution
    , completion = fileNameCompletion
    , action = liftCmd . doSaveAs
    }

-- | The modules command, lists modules in the current project
modulesCmd :: ViewerAction m u => Command u'(ViewerStateT m) 
modulesCmd = Command
    { helpLine = "modules: show a list of modules in the current project"
    , root = "modules"
    , parser = parseArity0 "modules"
    , isAllowed = hasOpenedSolution
    , completion = \_ -> return $ Just []
    , action = \_ -> liftCmd doModules
    }

-- | The module command, sets the currently open module
moduleCmd :: ViewerAction m u => Command u'(ViewerStateT m) 
moduleCmd = Command
    { helpLine = "module MODULE: set MODULE as the current module"
    , root = "module"
    , parser = parseArity1 "module" "module name"
    , isAllowed = hasOpenedSolution
    , completion = moduleNameCompletion
    , action = liftCmd . doModule
    }

-- | The declarations command, shows the declarations in the current module
declarationsCmd :: ViewerAction m u => Command u'(ViewerStateT m) 
declarationsCmd = Command
    { helpLine = "declarations: list the declarations in the current module"
    , root = "declarations"
    , parser = parseArity0 "declarations"
    , isAllowed = hasCurrentModule
    , completion = \_ -> return $  Just []
    , action = \_ -> liftCmd doDeclarations
    }

-- | The imports command, lists the import declaratiosn in the current module
importsCmd :: ViewerAction m u => Command u'(ViewerStateT m) 
importsCmd = Command
    { helpLine = "imports: list the imports in the current module"
    , root = "imports"
    , parser = parseArity0 "imports"
    , isAllowed = hasCurrentModule
    , completion = \_ -> return $  Just []
    , action = \_ -> liftCmd doImports
    }

-- | The imported command, lists all symbols imported by the current module
importedCmd :: ViewerAction m u => Command u'(ViewerStateT m) 
importedCmd = Command
    { helpLine = "imported: list the symbols imported by the current module"
    , root = "imported"
    , parser = parseArity0 "imported"
    , isAllowed = hasCurrentModule
    , completion = \_ -> return $  Just []
    , action = \_ -> liftCmd doImported
    }

-- | The exports command, lists all export declarations in the current module
exportsCmd :: ViewerAction m u => Command u'(ViewerStateT m) 
exportsCmd = Command
    { helpLine = "exports: list the exports in the current module"
    , root = "exports"
    , parser = parseArity0 "exports"
    , isAllowed = hasCurrentModule
    , completion = \_ -> return $  Just []
    , action = \_ -> liftCmd doExports
    }

-- | The exported command, lists all symbols exported by the current module
exportedCmd :: ViewerAction m u => Command u'(ViewerStateT m) 
exportedCmd = Command
    { helpLine = "exported: list the symbols exported by the current module"
    , root = "exported"
    , parser = parseArity0 "exported"
    , isAllowed = hasCurrentModule
    , completion = \_ -> return $  Just []
    , action = \_ -> liftCmd doExported
    }

-- | The visible command, lists all symbols visible at the top level of the current module
visibleCmd :: ViewerAction m u => Command u'(ViewerStateT m) 
visibleCmd = Command
    { helpLine = "visible: list the symbols visible at the top level of the current module"
    , root = "visible"
    , parser = parseArity0 "visible"
    , isAllowed = hasCurrentModule
    , completion = \_ -> return $  Just []
    , action = \_ -> liftCmd doVisible
    }

-- | The cat command, outputs the body of a declaration in the current module
catCmd :: ViewerAction m u => Command u'(ViewerStateT m) 
catCmd = Command
    { helpLine = "cat SYMBOL: show the body of the declaration of SYMBOL"
    , root = "cat"
    , parser = parseArity1 "cat" "symbol name"
    , isAllowed = hasCurrentModule
    , completion = declarationNameCompletion
    , action = liftCmd . doCat
    }

-- | The add module command, adds a new module to the project
addModuleCmd :: ViewerAction m u => Command u'(ViewerStateT m)
addModuleCmd = Command
    { helpLine = "add module MODULE: add a new module"
    , root = "add module"
    , parser = parseArity1 "add module" "module name"
    , isAllowed = hasOpenedSolution
    , completion = moduleNameCompletion
    , action = liftCmd . doAddModule
    }

-- | The remove module command, removes a module from the project
removeModuleCmd :: ViewerAction m u => Command u'(ViewerStateT m)
removeModuleCmd = Command
    { helpLine = "remove module MODULE: remove a module"
    , root = "add module"
    , parser = parseArity1 "remove module" "module name"
    , isAllowed = hasOpenedSolution
    , completion = moduleNameCompletion
    , action = liftCmd . doRemoveModule
    }

addExportCmd :: ViewerAction m u => Command u'(ViewerStateT m)
addExportCmd = Command
    { helpLine = "add export EXPORT: add an export"
    , root = "add export"
    , parser = parseArity1 "add export" "export"
    , isAllowed = hasCurrentModule
    , completion = declarationNameCompletion
    , action = liftCmd . doAddExport
    }

removeExportCmd :: ViewerAction m u => Command u'(ViewerStateT m)
removeExportCmd = Command
    { helpLine = "remove export EXPORT: add an export"
    , root = "remove export"
    , parser = parseArity1 "remove export" "export"
    , isAllowed = hasCurrentModule
    , completion = declarationNameCompletion
    , action = liftCmd . doRemoveExport
    }


addImportCmd :: ViewerAction m u => Command u'(ViewerStateT m)
addImportCmd = Command
    { helpLine = "add import IMPORT: add an import"
    , root = "add import"
    , parser = parseArity1 "add import" "import"
    , isAllowed = hasCurrentModule
    , completion = moduleNameCompletion
    , action = liftCmd . doAddImport
    }

removeImportCmd :: ViewerAction m u => Command u'(ViewerStateT m)
removeImportCmd = Command
    { helpLine = "remove import IMPORT: add an import"
    , root = "remove import"
    , parser = parseArity1 "remove import" "import"
    , isAllowed = hasCurrentModule
    , completion = moduleNameCompletion
    , action = liftCmd . doRemoveImport
    }

-- | The add decl command, opens the editor to have the user enter the text of a
-- new declaration to add
addDeclarationCmd :: (ViewerIOAction m u) 
                  => Editor m u
                  -> Command u'(ViewerStateT m)
addDeclarationCmd editor = Command
    { helpLine = "add decl: add a new declaration"
    , root = "add decl"
    , parser = parseArity0 "add decl"
    , isAllowed = hasCurrentModule
    , completion = \_ -> return $ Just []
    , action = \_ -> liftCmd $ doAddDeclaration editor
    }

-- | The remove decl command, removes a declaration from the current module
removeDeclarationCmd :: ViewerAction m u => Command u'(ViewerStateT m)
removeDeclarationCmd = Command
    { helpLine = "remove decl DECLARATION: remove a declaration"
    , root = "remove decl"
    , parser = parseArity1 "remove decl" "declaration"
    , isAllowed = hasCurrentModule
    , completion = declarationNameCompletion
    , action = liftCmd . doRemoveDeclaration
    }

-- | The edit command, opens the editor with the specified declaration
editCmd :: (ViewerIOAction m u) => Editor m u -> Command u'(ViewerStateT m)
editCmd editor = Command
    { helpLine = "edit SYMBOL: edit a declaration"
    , root = "edit"
    , parser = parseArity1 "edit" "symbol name"
    , isAllowed = hasCurrentModule
    , completion = declarationNameCompletion
    , action = liftCmd . doEdit editor
    }

buildCmd :: (ViewerIOAction m u) => Builder m u -> Command u'(ViewerStateT m)
buildCmd builder = Command
    { helpLine = "build: Build the project"
    , root = "build"
    , parser = parseArity0 "build"
    , isAllowed = hasOpenedSolution
    , completion = \_ -> return $ Just []
    , action = \_ -> liftCmd $ doBuild builder
    }

runCmd :: (ViewerIOAction m u) => Runner m u -> Command u'(ViewerStateT m)
runCmd runner = Command
    { helpLine = "run: Run the project executable"
    , root = "run"
    , parser = parseArity0 "run"
    , isAllowed = hasOpenedSolution
    , completion = \_ -> return $ Just []
    , action = \_ -> liftCmd $ doRun runner
    }

-- | The tree command, displays the project as a tree of modules and declarations
treeCmd :: ViewerAction m u => Command u'(ViewerStateT m) 
treeCmd = Command
    { helpLine = "tree: Display a tree of the current project's modules and declarations"
    , root = "tree"
    , parser = parseArity0 "tree"
    , isAllowed = hasOpenedSolution
    , completion = \_ -> return $  Just []
    , action = \_ -> liftCmd doTree
    }

-- | The search command, finds all symbols in the project which contain the search phrase
searchCmd :: ViewerAction m u => Command u'(ViewerStateT m)
searchCmd = Command
    { helpLine = "search SYMBOL: Search all modules for a declaration"
    , root = "search"
    , parser = parseArity1 "search" "search string"
    , isAllowed = hasOpenedSolution
    , completion = \_ -> return $ Just []
    , action = liftCmd . doSearch
    }

-- | The quit command, exits the program
quitCmd :: ViewerAction m u => Command u' (ViewerStateT m) 
quitCmd = Command
    { helpLine = "quit: exit the program"
    , root = "quit"
    , parser = parseArity0 "quit"
    , isAllowed = return True
    , completion = \_ -> return $  Just []
    , action = \_ -> doQuit
    }


          


