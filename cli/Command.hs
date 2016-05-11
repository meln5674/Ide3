{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
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

import Data.Maybe
import Data.List

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Writer
import Control.Monad.Trans.State.Strict

import System.Console.Haskeline

import Ide3.Mechanism
import Ide3.Monad


import qualified Ide3.Declaration as Declaration
import qualified Ide3.Module as Module

import Ide3.Types
    ( item
    , items
    , body
    , Module
    , ModuleInfo(..)
    , Symbol(..)
    , getChild
    , DeclarationInfo (..)
    , ProjectError (..)
    , ModuleChild (..)
    , Qualify (..)
    )

import Ide3.Mechanism.State()

import Command.Types
import Command.Trans

import ModuleTree
import CmdParser
import Viewer
import Editor
import Builder
import Runner

-- | Temporary, will be removed
type UserError = ()

-- | Run a project command, and return either the message or error it produced
printOnError :: ViewerMonad m 
             => ProjectResult (ViewerStateT m) UserError String 
             -> ViewerStateT m String
printOnError f = liftM (either show id) $ runExceptT f

-- | Perform some action which requires the current module open in the viewer
withSelectedModule :: (Show a, ViewerMonad m) 
                   => (Module -> ProjectResult (ViewerStateT m) UserError [Output a]) 
                   -> ViewerStateT m String
withSelectedModule f = printOnError $ do
    maybeName <- lift $ gets currentModule
    case maybeName of
        Nothing -> throwE $ InvalidOperation "No module currently selected" ""
        Just name -> do
            module_ <- getModule (ModuleInfo (Symbol name))
            xs <- f module_
            return $ intercalate "\n" $ map show xs

-- | Action for the help command
doHelp :: ViewerMonad m => CommandT u m String
doHelp = printHelp

doNew :: ViewerMonad m => FilePath -> ViewerStateT m String
doNew path = printOnError $ do
    createNewFile path
    return "Created"

-- | Action for the open command
doOpen :: (MonadIO m, ViewerMonad m) => FilePath -> ViewerStateT m String
doOpen path = printOnError $ do
    openProject path
    return "Loaded"

-- | Action for the save command
doSave :: (MonadIO m, ViewerMonad m) => ViewerStateT m String
doSave = printOnError $ do
    saveProject Nothing
    return "Saved"

-- | Action for the save as command
doSaveAs :: (MonadIO m, ViewerMonad m) => FilePath -> ViewerStateT m String
doSaveAs p = printOnError $ do
    saveProject $ Just p
    return "Saved"

-- | Action for the modules command
doModules :: ViewerMonad m => ViewerStateT m String
doModules = printOnError $ do
    names <- getModules
    return $ intercalate "\n" $ map show names

-- | Action for the module command
doModule :: ViewerMonad m => String -> ViewerStateT m String
doModule name = printOnError $ do
    _ <- getModule (ModuleInfo (Symbol name))
    lift $ modify $ \s -> s{currentModule=Just name}
    return ""

-- | Action for the declarations command
doDeclarations :: ViewerMonad m => ViewerStateT m String
doDeclarations = withSelectedModule 
    $ return 
    . asShows
    . map Declaration.info
    . items 
    . Module.getDeclarations

-- | Action for the imports command
doImports :: ViewerMonad m => ViewerStateT m String
doImports = withSelectedModule 
    $ return
    . asShows
    . items
    . Module.getImports

-- | Action for the imported command
doImported :: ViewerMonad m => ViewerStateT m String
doImported = withSelectedModule
    $ liftM asShows 
    . Module.importedSymbols

-- | Action for the exports command
doExports :: ViewerMonad m => ViewerStateT m String
doExports = withSelectedModule
    $ return
    . asShows
    . items
    . Module.getExports

-- | Action for the exported command
doExported :: ViewerMonad m => ViewerStateT m String
doExported = withSelectedModule
    $   (return 
    .   asShows
    .   map getChild)
    <=< Module.exportedSymbols 

-- | Action for the visible command
doVisible :: ViewerMonad m => ViewerStateT m String
doVisible = withSelectedModule $ liftM asShows . Module.internalSymbols

-- | Action for the cat command
doCat :: ViewerMonad m => String -> ViewerStateT m String
doCat sym = withSelectedModule $ \module_ -> ExceptT $ return $ do
    decl <- Module.getDeclaration module_ (DeclarationInfo (Symbol sym))
    let strings :: [Output Bool]
        strings = asStrings . lines . body . getChild $ decl
    return strings

-- | Action for the add module command
doAddModule :: ViewerMonad m => String -> ViewerStateT m String
doAddModule moduleName = printOnError $ do
    createModule (ModuleInfo (Symbol moduleName))
    return "Added"

-- | Action for the remove module command
doRemoveModule :: ViewerMonad m => String -> ViewerStateT m String
doRemoveModule moduleName = printOnError $ do
    removeModule (ModuleInfo (Symbol moduleName))
    return "Removed"

doAddExport :: ViewerMonad m => String -> ViewerStateT m String
doAddExport export = withSelectedModule $ \module_ -> do
    addRawExport (Module.info module_) export
    return [asString "Added" :: Output Bool]

doRemoveExport = undefined

doAddImport :: ViewerMonad m => String -> ViewerStateT m String
doAddImport import_ = withSelectedModule $ \module_ -> do
    addRawImport (Module.info module_) $ "import " ++ import_
    return [asString "Added" :: Output Bool]

doRemoveImport = undefined

-- | Action for the add decl command
doAddDeclaration :: (MonadIO m, ViewerMonad m) => (forall u . Editor m u) -> ViewerStateT m String
doAddDeclaration editor = withSelectedModule $ \module_ -> do
    newDecl <- ExceptT $ lift $ runExceptT $ runEditor editor ""
    case newDecl of
        EditConfirmed newBody -> do
            toAdd <- ExceptT $ return $ Declaration.parseAndCombine newBody Nothing
            addDeclaration (Module.info module_) toAdd
            return [asString "Added"]
        DeleteConfirmed -> error "Deleted a new declaration?"
        EditCanceled -> return [asString "Add canceled" :: Output Bool]

-- | Action for the remove decl command
doRemoveDeclaration :: ViewerMonad m => String -> ViewerStateT m String
doRemoveDeclaration sym = withSelectedModule $ \module_ -> do
    removeDeclaration (Module.info module_) (DeclarationInfo (Symbol sym))
    return [asString "Removed" :: Output Bool]

-- | Action for the edit command
doEdit :: (MonadIO m, ViewerMonad m) => (forall u . Editor m u) -> String -> ViewerStateT m String
doEdit editor sym = withSelectedModule $ \module_ -> do
    let declInfo = DeclarationInfo (Symbol sym)
    let moduleInfo = Module.info module_
    decl <- ExceptT $ return $ Module.getDeclaration module_ declInfo
    let declBody = body $ getChild decl
    newDeclBody <- ExceptT $ lift $ runExceptT $ runEditor editor declBody
    case newDeclBody of
        EditConfirmed newBody -> do 
            editDeclaration moduleInfo declInfo $ \_ -> Declaration.parseAndCombine newBody Nothing
            return [asShow "Edit completed"]
        DeleteConfirmed -> do
            removeDeclaration moduleInfo declInfo
            return [asShow "Delete completed"]
        EditCanceled -> return [asShow "Edit canceled"]

doBuild :: (MonadIO m, ViewerMonad m) => (forall u . Builder m u) -> ViewerStateT m String
doBuild builder = printOnError $ do
    prepareBuild
    r <- ExceptT $ lift $ runExceptT $ runBuilder builder
    case r of
        BuildSucceeded out err -> return $ out ++ err
        BuildFailed out err -> return $ out ++ err

doRun :: (MonadIO m, ViewerMonad m) => (forall u . Runner m u) -> ViewerStateT m String
doRun runner = printOnError $ do
    r <- ExceptT $ lift $ runExceptT $ runRunner runner
    case r of
        RunSucceeded out err -> return $ out ++ err
        RunFailed out err -> return $ out ++ err
    
-- | Action for the tree command
doTree :: ViewerMonad m => ViewerStateT m String
doTree = printOnError $ do
    trees <- makeTree
    return $ intercalate "\n \n" $ map formatTree trees
    --return $ show trees

-- | Action for the search command
doSearch :: ViewerMonad m => String -> ViewerStateT m String
doSearch sym = printOnError $ do
    modules <- getModules
    let matchingDeclsFrom info = do
            decls <- lift $ getDeclarations info
            let matches = filter (\(DeclarationInfo (Symbol sym')) -> sym `isInfixOf` sym') decls
                taggedMatches = map (ModuleChild info) matches
            tell taggedMatches
    matchingDecls <- execWriterT $ forM modules matchingDeclsFrom
    return $ intercalate "\n" $ map (show . qual) matchingDecls

-- | Action for the quit command
doQuit :: ViewerMonad m => CommandT u (ViewerStateT m) String
doQuit = do
    setExitFlag
    return "Exiting..."


-- | Given a prefix, find module names that have that prefix
moduleNameCompletion :: ViewerMonad m => String -> (ViewerStateT m) (Maybe [Completion])
moduleNameCompletion s = do
    p <- hasOpenedProject
    if not p
        then return Nothing
        else do
            r <- runExceptT $ do
                mods <- getModules
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
declarationNameCompletion :: ViewerMonad m => String -> (ViewerStateT m) (Maybe [Completion])
declarationNameCompletion s = do
    mayben <- gets currentModule
    case mayben of
        Nothing -> return Nothing
        Just n -> do
            r <- runExceptT $ do
                m <- getModule (ModuleInfo (Symbol n))
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




-- | The help command, prints the help lines for all other commands
helpCmd :: ViewerMonad m => Command u (ViewerStateT m) 
helpCmd = Command
    { helpLine = "help: show this message"
    , root = "help"
    , parser = parseArity0 "help"
    , isAllowed = return True
    , completion = \_ -> return $ Just []
    , action = \_ -> doHelp
    }

newCmd :: (MonadIO m, ViewerMonad m) => Command u (ViewerStateT m)
newCmd = Command
    { helpLine = "new: create a new project"
    , root = "new"
    , parser = parseArity1 "new" "path"
    , isAllowed = return True
    , completion = \_ ->    return $ Just []
    , action = liftCmd . doNew
    }

-- | The open command, takes a path and opens the project at that file or 
-- directory
openCmd :: (MonadIO m, ViewerMonad m) => Command u (ViewerStateT m) 
openCmd = Command
    { helpLine = "open PATH: open a project rooted at PATH"
    , root = "open"
    , parser = parseArity1 "open" "directory path"
    , isAllowed = return True
    , completion =  liftM Just . listFiles
    , action = liftCmd . doOpen
    }

-- | The save command, saves the project at whatever path was last used to save
saveCmd :: (MonadIO m, ViewerMonad m) => Command u (ViewerStateT m)  
saveCmd = Command
    { helpLine = "save: save the project at the current path"
    , root = "save"
    , parser = parseArity0 "save"
    , isAllowed = hasOpenedProject
    , completion = liftM Just . listFiles
    , action = \_ -> liftCmd doSave
    }

-- | The save as command, saves the project at the specified path
saveAsCmd :: (MonadIO m, ViewerMonad m) => Command u (ViewerStateT m) 
saveAsCmd = Command
    { helpLine = "save as PATH: save the project at PATH"
    , root = "save as"
    , parser = parseArity1 "save as" "path"
    , isAllowed = hasOpenedProject
    , completion = liftM Just . listFiles
    , action = liftCmd . doSaveAs
    }

-- | The modules command, lists modules in the current project
modulesCmd :: ViewerMonad m => Command u (ViewerStateT m) 
modulesCmd = Command
    { helpLine = "modules: show a list of modules in the current project"
    , root = "modules"
    , parser = parseArity0 "modules"
    , isAllowed = hasOpenedProject
    , completion = \_ -> return $ Just []
    , action = \_ -> liftCmd doModules
    }

-- | The module command, sets the currently open module
moduleCmd :: ViewerMonad m => Command u (ViewerStateT m) 
moduleCmd = Command
    { helpLine = "module MODULE: set MODULE as the current module"
    , root = "module"
    , parser = parseArity1 "module" "module name"
    , isAllowed = hasOpenedProject
    , completion = moduleNameCompletion
    , action = liftCmd . doModule
    }

-- | The declarations command, shows the declarations in the current module
declarationsCmd :: ViewerMonad m => Command u (ViewerStateT m) 
declarationsCmd = Command
    { helpLine = "declarations: list the declarations in the current module"
    , root = "declarations"
    , parser = parseArity0 "declarations"
    , isAllowed = hasCurrentModule
    , completion = \_ -> return $  Just []
    , action = \_ -> liftCmd doDeclarations
    }

-- | The imports command, lists the import declaratiosn in the current module
importsCmd :: ViewerMonad m => Command u (ViewerStateT m) 
importsCmd = Command
    { helpLine = "imports: list the imports in the current module"
    , root = "imports"
    , parser = parseArity0 "imports"
    , isAllowed = hasCurrentModule
    , completion = \_ -> return $  Just []
    , action = \_ -> liftCmd doImports
    }

-- | The imported command, lists all symbols imported by the current module
importedCmd :: ViewerMonad m => Command u (ViewerStateT m) 
importedCmd = Command
    { helpLine = "imported: list the symbols imported by the current module"
    , root = "imported"
    , parser = parseArity0 "imported"
    , isAllowed = hasCurrentModule
    , completion = \_ -> return $  Just []
    , action = \_ -> liftCmd doImported
    }

-- | The exports command, lists all export declarations in the current module
exportsCmd :: ViewerMonad m => Command u (ViewerStateT m) 
exportsCmd = Command
    { helpLine = "exports: list the exports in the current module"
    , root = "exports"
    , parser = parseArity0 "exports"
    , isAllowed = hasCurrentModule
    , completion = \_ -> return $  Just []
    , action = \_ -> liftCmd doExports
    }

-- | The exported command, lists all symbols exported by the current module
exportedCmd :: ViewerMonad m => Command u (ViewerStateT m) 
exportedCmd = Command
    { helpLine = "exported: list the symbols exported by the current module"
    , root = "exported"
    , parser = parseArity0 "exported"
    , isAllowed = hasCurrentModule
    , completion = \_ -> return $  Just []
    , action = \_ -> liftCmd doExported
    }

-- | The visible command, lists all symbols visible at the top level of the current module
visibleCmd :: ViewerMonad m => Command u (ViewerStateT m) 
visibleCmd = Command
    { helpLine = "visible: list the symbols visible at the top level of the current module"
    , root = "visible"
    , parser = parseArity0 "visible"
    , isAllowed = hasCurrentModule
    , completion = \_ -> return $  Just []
    , action = \_ -> liftCmd doVisible
    }

-- | The cat command, outputs the body of a declaration in the current module
catCmd :: ViewerMonad m => Command u (ViewerStateT m) 
catCmd = Command
    { helpLine = "cat SYMBOL: show the body of the declaration of SYMBOL"
    , root = "cat"
    , parser = parseArity1 "cat" "symbol name"
    , isAllowed = hasCurrentModule
    , completion = declarationNameCompletion
    , action = liftCmd . doCat
    }

-- | The add module command, adds a new module to the project
addModuleCmd :: ViewerMonad m => Command u (ViewerStateT m)
addModuleCmd = Command
    { helpLine = "add module MODULE: add a new module"
    , root = "add module"
    , parser = parseArity1 "add module" "module name"
    , isAllowed = hasOpenedProject
    , completion = moduleNameCompletion
    , action = liftCmd . doAddModule
    }

-- | The remove module command, removes a module from the project
removeModuleCmd :: ViewerMonad m => Command u (ViewerStateT m)
removeModuleCmd = Command
    { helpLine = "remove module MODULE: remove a module"
    , root = "add module"
    , parser = parseArity1 "remove module" "module name"
    , isAllowed = hasOpenedProject
    , completion = moduleNameCompletion
    , action = liftCmd . doRemoveModule
    }

addExportCmd :: ViewerMonad m => Command u (ViewerStateT m)
addExportCmd = Command
    { helpLine = "add export EXPORT: add an export"
    , root = "add export"
    , parser = parseArity1 "add export" "export"
    , isAllowed = hasCurrentModule
    , completion = declarationNameCompletion
    , action = liftCmd . doAddExport
    }

removeExportCmd :: ViewerMonad m => Command u (ViewerStateT m)
removeExportCmd = Command
    { helpLine = "remove export EXPORT: add an export"
    , root = "remove export"
    , parser = parseArity1 "remove export" "export"
    , isAllowed = hasCurrentModule
    , completion = declarationNameCompletion
    , action = liftCmd . doRemoveExport
    }


addImportCmd :: ViewerMonad m => Command u (ViewerStateT m)
addImportCmd = Command
    { helpLine = "add import IMPORT: add an import"
    , root = "add import"
    , parser = parseArity1 "add import" "import"
    , isAllowed = hasCurrentModule
    , completion = moduleNameCompletion
    , action = liftCmd . doAddImport
    }

removeImportCmd :: ViewerMonad m => Command u (ViewerStateT m)
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
addDeclarationCmd :: (MonadIO m, ViewerMonad m) 
                  => (forall u . Editor m u) 
                  -> Command u (ViewerStateT m)
addDeclarationCmd editor = Command
    { helpLine = "add decl: add a new declaration"
    , root = "add decl"
    , parser = parseArity0 "add decl"
    , isAllowed = hasCurrentModule
    , completion = \_ -> return $ Just []
    , action = \_ -> liftCmd $ doAddDeclaration editor
    }

-- | The remove decl command, removes a declaration from the current module
removeDeclarationCmd :: ViewerMonad m => Command u (ViewerStateT m)
removeDeclarationCmd = Command
    { helpLine = "remove decl DECLARATION: remove a declaration"
    , root = "remove decl"
    , parser = parseArity1 "remove decl" "declaration"
    , isAllowed = hasCurrentModule
    , completion = declarationNameCompletion
    , action = liftCmd . doRemoveDeclaration
    }

-- | The edit command, opens the editor with the specified declaration
editCmd :: (MonadIO m, ViewerMonad m) => (forall u . Editor m u) -> Command u (ViewerStateT m)
editCmd editor = Command
    { helpLine = "edit SYMBOL: edit a declaration"
    , root = "edit"
    , parser = parseArity1 "edit" "symbol name"
    , isAllowed = hasCurrentModule
    , completion = declarationNameCompletion
    , action = liftCmd . doEdit editor
    }

buildCmd :: (MonadIO m, ViewerMonad m) => (forall u . Builder m u) -> Command u (ViewerStateT m)
buildCmd builder = Command
    { helpLine = "build: Build the project"
    , root = "build"
    , parser = parseArity0 "build"
    , isAllowed = hasOpenedProject
    , completion = \_ -> return $ Just []
    , action = \_ -> liftCmd $ doBuild builder
    }

runCmd :: (MonadIO m, ViewerMonad m) => (forall u . Runner m u) -> Command u (ViewerStateT m)
runCmd runner = Command
    { helpLine = "run: Run the project executable"
    , root = "run"
    , parser = parseArity0 "run"
    , isAllowed = hasOpenedProject
    , completion = \_ -> return $ Just []
    , action = \_ -> liftCmd $ doRun runner
    }

-- | The tree command, displays the project as a tree of modules and declarations
treeCmd :: ViewerMonad m => Command u (ViewerStateT m) 
treeCmd = Command
    { helpLine = "tree: Display a tree of the current project's modules and declarations"
    , root = "tree"
    , parser = parseArity0 "tree"
    , isAllowed = hasOpenedProject
    , completion = \_ -> return $  Just []
    , action = \_ -> liftCmd doTree
    }

-- | The search command, finds all symbols in the project which contain the search phrase
searchCmd :: ViewerMonad m => Command u (ViewerStateT m)
searchCmd = Command
    { helpLine = "search SYMBOL: Search all modules for a declaration"
    , root = "search"
    , parser = parseArity1 "search" "search string"
    , isAllowed = hasOpenedProject
    , completion = \_ -> return $ Just []
    , action = liftCmd . doSearch
    }

-- | The quit command, exits the program
quitCmd :: ViewerMonad m => Command u (ViewerStateT m) 
quitCmd = Command
    { helpLine = "quit: exit the program"
    , root = "quit"
    , parser = parseArity0 "quit"
    , isAllowed = return True
    , completion = \_ -> return $  Just []
    , action = \_ -> doQuit
    }


          


