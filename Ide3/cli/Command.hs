{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
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

import System.Process
import System.Directory

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Except
import Control.Monad.Trans.Writer

import System.Console.Haskeline

import Ide3.NewMonad
import Ide3.NewMonad.Utils


import qualified Ide3.Declaration as Declaration
import qualified Ide3.Module as Module

import Ide3.Utils
import Ide3.Types
    ( item
    , items
    , body
    , ProjectInfo(..)
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

import Ide3.ModuleTree


import Command.Types
import Command.Trans

import Args
import CmdParser
import Viewer
import Editor
import Builder
import Runner
import Initializer
import ProjectInitializer

type UserError = ()

type ViewerAction m =
    ( ViewerMonad m
    , SolutionClass m
    , ProjectModuleClass m
    , ProjectExternModuleClass m
    , ModuleImportClass m
    , ModuleExportClass m
    , ModuleDeclarationClass m
    , ModulePragmaClass m
    , ExternModuleExportClass m
    , ViewerPersistToken m ~ PersistToken m
    )
type ViewerIOAction m = (ViewerAction m, MonadIO m)

-- | Run a project command, and return either the message or error it produced
printOnError :: ( ViewerAction m
                )
             => SolutionResult UserError (ViewerStateT m) String 
             -> ViewerStateT m String
printOnError f = liftM (either show id) $ runExceptT f

-- | Perform an action only if there is an selected project, otherwise return
-- an error
withSelectedProject :: (Show a, ViewerAction m)
                    => (ProjectInfo -> SolutionResult UserError (ViewerStateT m) [Output a])
                    -> ViewerStateT m String
withSelectedProject f = printOnError $ do
    maybeProjectInfo <- lift $ getCurrentProject
    case maybeProjectInfo of
        Nothing -> throwE $ InvalidOperation "No project currently selected" ""
        Just pji -> liftM processOutputs $ f pji

-- | Perform an action only if there is a selected module, other return an error
withSelectedModuleInfo :: ( Show a
                          , ViewerAction m
                          )
                       => (ProjectInfo -> ModuleInfo -> SolutionResult UserError (ViewerStateT m) [Output a])
                       -> ViewerStateT m String
withSelectedModuleInfo f = printOnError $ do
    maybeModuleInfo <- lift $ getCurrentModule
    case maybeModuleInfo of
        Nothing -> throwE $ InvalidOperation "No module currently selected" ""
        Just (pji,mi) -> liftM processOutputs $ f pji mi


-- | Action for the help command
doHelp :: ViewerAction m => CommandT u' (ViewerStateT m) String
doHelp = printHelp

-- | Action for the shell command
doShell :: ViewerIOAction m => String -> ViewerStateT m String
doShell cmd = printOnError $ do
    let p = shell cmd
    (_, out, err) <- liftIO $ readCreateProcessWithExitCode p ""
    return $ out ++ err

-- | Action for the cd command
doCd :: ViewerIOAction m => FilePath -> ViewerStateT m String
doCd path = printOnError $ do
    liftIO $ setCurrentDirectory path
    return ""

-- | Action for the new command
doNew :: (ViewerAction m, PersistenceClass m, Args a) => Initializer a m -> String -> ViewerStateT m String
doNew initializer arg = printOnError $ do
    case runInitializerWithInput initializer =<< (parseArityN arg) of
        Left msg -> return msg
        Right initializerAction -> do
            r <- ExceptT $ lift $ runExceptT $ initializerAction
            case r of
                InitializerSucceeded out err tok -> do
                    load tok
                    return $ out ++ err
                InitializerFailed out err -> return $ out ++ err

-- | Action for the open command
doOpen :: forall m
        . ( ViewerIOAction m
          , PersistenceClass m
          ) 
       => FilePath 
       -> ViewerStateT m String
doOpen path = printOnError $ do
    openSolution path -- 1:: SolutionResult (ViewerStateT m 
    return "Loaded"

-- | Action for the save command
doSave :: ( ViewerIOAction m
          , PersistenceClass m
          ) 
       => ViewerStateT m String
doSave = printOnError $ do
    saveSolution Nothing
    return "Saved"

-- | Action for the save as command
doSaveAs :: ( ViewerIOAction m
            , PersistenceClass m
            ) 
         => FilePath 
         -> ViewerStateT m String
doSaveAs p = printOnError $ do
    saveSolution $ Just p
    return "Saved"

-- | Action for the projects command
doProjects :: ( ViewerAction m
              ) 
           => ViewerStateT m String
doProjects = printOnError $ liftM (intercalate "\n" . map show) $ bounce getProjects

-- | Action for the project command
doProject :: ( ViewerAction m
             ) 
          => String 
          -> ViewerStateT m String
doProject n = printOnError $ do
    lift $ setCurrentProject (ProjectInfo n)
    return ""
    
-- | Action for the add project command
doAddProject :: ( ViewerAction m
                , Args a
                )
             => ProjectInitializer a m
             -> String 
             -> ViewerStateT m String
doAddProject projectInitializer arg = printOnError $ do
    case runProjectInitializerWithInput projectInitializer =<< parseArityN arg of
        Left msg -> return msg
        Right initializerAction -> do
            r <- ExceptT $ lift $ runExceptT $ initializerAction
            case r of
                ProjectInitializerSucceeded out err _ -> return $ out ++ err
                ProjectInitializerFailed out err -> return $ out ++ err

-- | Action for the remove project command
doRemoveProject :: ( ViewerAction m
                   ) 
                => String 
                -> ViewerStateT m String
doRemoveProject n = printOnError $ do
    bounce $ removeProject (ProjectInfo n)
    return "Removed"


-- | Action for the modules command
doModules :: ( ViewerAction m
             )
          => ViewerStateT m String
doModules = withSelectedProject $ liftM asShows . bounce . getModules

-- | Action for the module command
doModule :: ( ViewerAction m 
            )
         => String 
         -> ViewerStateT m String
doModule name = withSelectedProject $ \pji -> do
    let info = (ModuleInfo (Symbol name))
    -- _ <- bounce $ getModule pji info
    lift $ setCurrentModule pji info
    return $ defaultOutputs []

-- | Action for the declarations command
doDeclarations :: ( ViewerAction m 
                  )
               => ViewerStateT m String
doDeclarations = withSelectedModuleInfo $ \pji mi -> do
    ds <- bounce $ getDeclarations pji mi >>= mapM (getDeclaration pji mi)
    return $ asShows $ map (Declaration.info . item) ds

-- | Action for the imports command
doImports :: ( ViewerAction m 
             )
          => ViewerStateT m String
doImports = withSelectedModuleInfo $ \pji mi -> do
    is <- bounce $ getImports pji mi >>= mapM (getImport pji mi)
    return $ asShows is

-- | Action for the imported command
doImported :: ( ViewerAction m 
              )
           => ViewerStateT m String
doImported = withSelectedModuleInfo $ \pji mi -> 
      liftM asShows 
    $ bounce
    $ Module.importedSymbols' pji mi

-- | Action for the exports command
doExports :: ( ViewerAction m 
             )
          => ViewerStateT m String
doExports = withSelectedModuleInfo $ \pji mi -> bounce $ do
    result <- getExports pji mi
    case result of
        Nothing -> return [asString "Exports everything"]
        Just es -> liftM (asShows . items) $ mapM (getExport pji mi) es

-- | Action for the exported command
doExported :: ( ViewerAction m 
              )
           => ViewerStateT m String
doExported = withSelectedModuleInfo $ \pji mi -> 
      liftM (asShows . map getChild)
    $ bounce 
    $ Module.exportedSymbols' pji mi

-- | Action for the visible command
doVisible :: ( ViewerAction m 
             )
          => ViewerStateT m String
doVisible = withSelectedModuleInfo $ \pji mi -> liftM asShows $ bounce $ Module.internalSymbols' pji mi

-- | Action for the cat command
doCat :: ( ViewerAction m 
         )
      => String 
      -> ViewerStateT m String
doCat sym = withSelectedModuleInfo $ \pji mi -> do
    decl <- bounce $ getDeclaration pji mi $ DeclarationInfo $ Symbol sym
    return $ defaultOutputs . asStrings . lines . body $ decl

-- | Action for the add module command
doAddModule :: ( ViewerAction m 
               )
            => String 
            -> ViewerStateT m String
doAddModule moduleName = withSelectedProject $ \pji -> do
    bounce $ createModule pji (ModuleInfo (Symbol moduleName))
    return $ defaultOutputs [asString "Added"]

-- | Action for the remove module command
doRemoveModule :: ( ViewerAction m 
                  )
               => String 
               -> ViewerStateT m String
doRemoveModule moduleName = withSelectedProject $ \pji -> do
    bounce $ removeModule pji (ModuleInfo (Symbol moduleName))
    return $ defaultOutputs $ [asString "Removed"]

-- | Action for the add export command
doAddExport :: ( ViewerAction m 
               )
            => String 
            -> ViewerStateT m String
doAddExport export = withSelectedModuleInfo $ \pji mi -> do
    _ <- bounce $ addRawExport pji mi export
    return [asString "Added" :: Output Bool]

-- | Action for the remove export command
doRemoveExport :: ( ViewerAction m 
                  )
               => String 
               -> ViewerStateT m String
doRemoveExport = undefined

-- | Action for the add import command
doAddImport :: ( ViewerAction m 
               )
            => String 
            -> ViewerStateT m String
doAddImport import_ = withSelectedModuleInfo $ \pji mi -> do
    _ <- bounce $ addRawImport pji mi $ "import " ++ import_
    return [asString "Added" :: Output Bool]

-- | Action for the remove import command
doRemoveImport :: ( ViewerAction m 
                  ) 
               => String 
               -> ViewerStateT m String
doRemoveImport = undefined

-- | Action for the add decl command
doAddDeclaration :: ( ViewerAction m
                    ) 
                 => Editor m
                 -> ViewerStateT m String
doAddDeclaration editor = withSelectedModuleInfo $ \pji mi -> do
    newDecl <- ExceptT $ lift $ runExceptT $ runEditor editor ""
    case newDecl of
        EditConfirmed newBody -> do
            toAdd <- ExceptT $ return $ Declaration.parseAndCombine newBody Nothing
            bounce $ addDeclaration pji mi toAdd
            return [asString "Added"]
        DeleteConfirmed -> error "Deleted a new declaration?"
        EditCanceled -> return [asString "Add canceled" :: Output Bool]

-- | Action for the remove decl command
doRemoveDeclaration :: ( ViewerAction m 
                       )
                    => String 
                    -> ViewerStateT m String
doRemoveDeclaration sym = withSelectedModuleInfo $ \pji mi -> do
    bounce $ removeDeclaration pji mi $ DeclarationInfo $ Symbol sym
    return [asString "Removed" :: Output Bool]

-- | Action for the edit command
doEdit :: forall m
        . ( ViewerAction m
          ) 
       => Editor m
       -> String 
       -> ViewerStateT m String
doEdit editor sym = withSelectedModuleInfo $ \pji mi -> do
    let declInfo = DeclarationInfo (Symbol sym)
    decl <- bounce $ getDeclaration pji mi declInfo
    let declBody = body decl
    newDeclBody <- ExceptT $ lift $ runExceptT $ runEditor editor declBody
    case newDeclBody of
        EditConfirmed newBody -> do 
            let r = Declaration.parseAndCombineLenient newBody Nothing declInfo
            case r of
                Right decl' -> do
                    void $ bounce 
                         $ editDeclaration pji mi declInfo 
                         $ const
                         $ Right decl'
                    return [asShow "Edit completed"]
                Left (decl',err) -> do
                    void $ bounce 
                         $ editDeclaration pji mi declInfo 
                         $ const
                         $ Right decl'
                    return [asShow $ show (err :: SolutionError UserError)]
        DeleteConfirmed -> do
            bounce $ removeDeclaration pji mi declInfo
            return [asShow "Delete completed"]
        EditCanceled -> return [asShow "Edit canceled"]

-- | Action for the build command
doBuild :: ( ViewerIOAction m
           ) 
        => Builder m
        -> ViewerStateT m String
doBuild builder = printOnError $ do
    bounce prepareBuild
    r <- ExceptT $ lift $ runExceptT $ runBuilder builder
    case r of
        BuildSucceeded contents _ -> return $ contents
        BuildFailed contents _ -> return $ contents

-- | Action for the run command
doRun :: ( ViewerIOAction m
         ) 
      => Runner m
      -> ViewerStateT m String
doRun runner = printOnError $ do
    maybePji <- lift $ getCurrentProject
    case maybePji of
        Nothing -> throwE $ InvalidOperation "No project selected" ""
        Just pji -> do
            r <- ExceptT $ lift $ runExceptT $ runRunner runner pji
            case r of
                RunSucceeded out err -> return $ out ++ err
                RunFailed out err -> return $ out ++ err
    
-- | Action for the tree command
doTree :: ( ViewerAction m 
          )
       => ViewerStateT m String
doTree = withSelectedProject $ \pji -> do
    trees <- bounce $ makeTree pji
    return $ defaultOutputs $ map (asString . formatTree) trees
    --return $ show trees

-- | Action for the search command
doSearch :: ( ViewerAction m 
            )
         => String 
         -> ViewerStateT m String
doSearch sym = printOnError $ do
    projects <- bounce getProjects
    modules <- liftM concat
        $ forM projects $ \pji -> liftM (map (\mi -> (pji,mi))) $ bounce $ getModules pji
    let matchingDeclsFrom pji mi = do
            decls <- lift $ bounce $ getDeclarations pji mi
            let matches = filter (\(DeclarationInfo (Symbol sym')) -> sym `isInfixOf` sym') decls
                taggedMatches = map (ProjectChild pji . ModuleChild mi) matches
            tell taggedMatches
    matchingDecls <- execWriterT $ forM modules $ uncurry matchingDeclsFrom
    return $ intercalate "\n" $ map (show . fmap qual) matchingDecls

-- | Action for the quit command
doQuit :: ViewerAction m => CommandT u' (ViewerStateT m) String
doQuit = do
    setExitFlag
    return "Exiting..."

-- | Do completion with the files and directories in the current working
-- directory
fileNameCompletion :: ( ViewerIOAction m 
                      )
                   => String 
                   -> (ViewerStateT m) (Maybe [Completion])
fileNameCompletion s = do
    comps <- listFiles s
    return $ Just $ flip map comps $ \comp -> comp{replacement = drop (length s) $ replacement comp }

-- | Do completion with the names of the projects loaded
projectNameCompletion :: ( ViewerAction m 
                         )
                      => String 
                      -> (ViewerStateT m) (Maybe [Completion])
projectNameCompletion s = do
    r <- runExceptT $ do
        pjis <- bounce getProjects
        let projectNames = flip map pjis $ \(ProjectInfo n) -> n
            matchingNames = filter (s `isPrefixOf`) projectNames
        return $ Just $ flip map matchingNames $ \n -> 
            Completion{ replacement = drop (length s) n
                      , display = n
                      , isFinished = not $ any (\n' -> n `isPrefixOf` n' && n /= n') matchingNames
                      }
    return $ case r of
        Right x -> x
        Left _ -> Nothing


-- | Do completion with the names of the modules in the currently selected project
moduleNameCompletion :: ( ViewerAction m 
                        )
                     => String 
                     -> (ViewerStateT m) (Maybe [Completion])
moduleNameCompletion s = do
    result <- getCurrentProject
    case result of
        Nothing -> return Nothing
        Just pji -> do
            r <- runExceptT $ do
                mods <- bounce $ getModules pji
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

-- | Do completion for the declarations in the currently selected module
declarationNameCompletion :: ( ViewerAction m 
                             )
                          => String 
                          -> (ViewerStateT m) (Maybe [Completion])
declarationNameCompletion s = do
    result <- getCurrentModule
    case result of
        Just (pji, mi) -> do
            r <- runExceptT $ do
                infos <- bounce $ getDeclarations pji mi
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
helpCmd :: ( ViewerAction m 
           )
        => Command u' (ViewerStateT m) 
helpCmd = Command
    { helpLine = "help: show this message"
    , root = "help"
    , parser = parseArity0 "help"
    , isAllowed = return True
    , completion = \_ -> return $ Just []
    , action = \_ -> doHelp
    }

-- | The shell command, executes a command in the system shell
shellCmd :: ( ViewerIOAction m 
            )
         => Command u' (ViewerStateT m)
shellCmd = Command
    { helpLine = "shell CMD: execute a command in the shell"
    , root = "shell"
    , parser = parseArity1 "shell" "command"
    , isAllowed = return True
    , completion = \_ -> return $ Just []
    , action = liftCmd . doShell
    }

-- | The cd command, changes the current working directory
cdCmd :: ( ViewerIOAction m 
         )
      => Command u' (ViewerStateT m)
cdCmd = Command
    { helpLine = "cd PATH: Change the current directory"
    , root = "cd"
    , parser = parseArity1 "cd" "path"
    , isAllowed = return True
    , completion = fileNameCompletion
    , action = liftCmd . doCd
    }

-- | The new command, creates a new solution using the provided initializer
newCmd :: ( ViewerIOAction m
          , Args a
--          , Show u
          , PersistenceClass m
          ) 
       => Initializer a m -> Command u' (ViewerStateT m)
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
openCmd :: ( ViewerIOAction m
           , PersistenceClass m
           ) 
        => Command u' (ViewerStateT m) 
openCmd = Command
    { helpLine = "open PATH: open a project rooted at PATH"
    , root = "open"
    , parser = parseArity1 "open" "directory path"
    , isAllowed = return True
    , completion =  fileNameCompletion
    , action = liftCmd . doOpen
    }

-- | The save command, saves the solution
saveCmd :: ( ViewerIOAction m
           , PersistenceClass m
           ) 
        => Command u' (ViewerStateT m)  
saveCmd = Command
    { helpLine = "save: save the project at the current path"
    , root = "save"
    , parser = parseArity0 "save"
    , isAllowed = lift hasOpenedSolution
    , completion = fileNameCompletion
    , action = \_ -> liftCmd doSave
    }

-- | The save as command, saves the project at the specified path
saveAsCmd :: ( ViewerIOAction m
             , PersistenceClass m
             ) 
          => Command u' (ViewerStateT m) 
saveAsCmd = Command
    { helpLine = "save as PATH: save the project at PATH"
    , root = "save as"
    , parser = parseArity1 "save as" "path"
    , isAllowed = lift hasOpenedSolution
    , completion = fileNameCompletion
    , action = liftCmd . doSaveAs
    }

-- | The projects command, lists availible projects
projectsCmd :: ( ViewerIOAction m
               ) 
            => Command u' (ViewerStateT m)
projectsCmd = Command
    { helpLine = "projects: show list of modules in the current solution"
    , root = "projects"
    , parser = parseArity0 "projects"
    , isAllowed = lift hasOpenedSolution
    , completion = const $ return $ Just []
    , action = const $ liftCmd doProjects
    }

-- | The project command, selects a project
projectCmd :: ( ViewerIOAction m
              ) 
           => Command u' (ViewerStateT m)
projectCmd = Command
    { helpLine = "project PROJECT: set PROJECT as the current project"
    , root = "project"
    , parser = parseArity1 "project" "project name"
    , isAllowed = lift hasOpenedSolution
    , completion = projectNameCompletion
    , action = liftCmd . doProject
    }

-- | The add project command, creates a new project using the provided project
-- initializer
addProjectCmd :: ( ViewerIOAction m
                 , Args a
--                 , Show u
                 ) 
              => ProjectInitializer a m
              -> Command u' (ViewerStateT m)
addProjectCmd projectInitializer = Command
    { helpLine = "add project [ARGS]...: add a new project"
    , root = "add project"
    , parser = parseArity1 "add project" "arguments"
    , isAllowed = lift hasOpenedSolution
    , completion = const $ return $ Just []
    , action = liftCmd . doAddProject projectInitializer
    }

-- | The remove project command, removes a project
removeProjectCmd :: ( ViewerIOAction m
                    ) 
                 => Command u' (ViewerStateT m)
removeProjectCmd = Command
    { helpLine = "remove project PROJECT: remove a project"
    , root = "remove project"
    , parser = parseArity1 "remove project" "project name"
    , isAllowed = lift hasOpenedSolution
    , completion = projectNameCompletion
    , action = liftCmd . doRemoveProject
    }

-- | The modules command, lists modules in the current project
modulesCmd :: ( ViewerAction m
              )
           => Command u' (ViewerStateT m) 
modulesCmd = Command
    { helpLine = "modules: show a list of modules in the current project"
    , root = "modules"
    , parser = parseArity0 "modules"
    , isAllowed = hasCurrentProject
    , completion = \_ -> return $ Just []
    , action = \_ -> liftCmd doModules
    }

-- | The module command, sets the currently open module
moduleCmd :: ( ViewerAction m 
             ) 
          => Command u' (ViewerStateT m) 
moduleCmd = Command
    { helpLine = "module MODULE: set MODULE as the current module"
    , root = "module"
    , parser = parseArity1 "module" "module name"
    , isAllowed = hasCurrentProject
    , completion = moduleNameCompletion
    , action = liftCmd . doModule
    }

-- | The declarations command, shows the declarations in the current module
declarationsCmd :: ( ViewerAction m 
                   )
                => Command u' (ViewerStateT m) 
declarationsCmd = Command
    { helpLine = "declarations: list the declarations in the current module"
    , root = "declarations"
    , parser = parseArity0 "declarations"
    , isAllowed = hasCurrentModule
    , completion = \_ -> return $  Just []
    , action = \_ -> liftCmd doDeclarations
    }

-- | The imports command, lists the import declaratiosn in the current module
importsCmd :: ( ViewerAction m
              )
           => Command u' (ViewerStateT m) 
importsCmd = Command
    { helpLine = "imports: list the imports in the current module"
    , root = "imports"
    , parser = parseArity0 "imports"
    , isAllowed = hasCurrentModule
    , completion = \_ -> return $  Just []
    , action = \_ -> liftCmd doImports
    }

-- | The imported command, lists all symbols imported by the current module
importedCmd :: ( ViewerAction m
               )
            => Command u' (ViewerStateT m) 
importedCmd = Command
    { helpLine = "imported: list the symbols imported by the current module"
    , root = "imported"
    , parser = parseArity0 "imported"
    , isAllowed = hasCurrentModule
    , completion = \_ -> return $  Just []
    , action = \_ -> liftCmd doImported
    }

-- | The exports command, lists all export declarations in the current module
exportsCmd :: ( ViewerAction m
              )
           => Command u' (ViewerStateT m) 
exportsCmd = Command
    { helpLine = "exports: list the exports in the current module"
    , root = "exports"
    , parser = parseArity0 "exports"
    , isAllowed = hasCurrentModule
    , completion = \_ -> return $  Just []
    , action = \_ -> liftCmd doExports
    }

-- | The exported command, lists all symbols exported by the current module
exportedCmd :: ( ViewerAction m 
               )
            => Command u' (ViewerStateT m) 
exportedCmd = Command
    { helpLine = "exported: list the symbols exported by the current module"
    , root = "exported"
    , parser = parseArity0 "exported"
    , isAllowed = hasCurrentModule
    , completion = \_ -> return $  Just []
    , action = \_ -> liftCmd doExported
    }

-- | The visible command, lists all symbols visible at the top level of the current module
visibleCmd :: ( ViewerAction m 
              )
           => Command u' (ViewerStateT m) 
visibleCmd = Command
    { helpLine = "visible: list the symbols visible at the top level of the current module"
    , root = "visible"
    , parser = parseArity0 "visible"
    , isAllowed = hasCurrentModule
    , completion = \_ -> return $  Just []
    , action = \_ -> liftCmd doVisible
    }

-- | The cat command, outputs the body of a declaration in the current module
catCmd :: ( ViewerAction m 
          )
       => Command u' (ViewerStateT m) 
catCmd = Command
    { helpLine = "cat SYMBOL: show the body of the declaration of SYMBOL"
    , root = "cat"
    , parser = parseArity1 "cat" "symbol name"
    , isAllowed = hasCurrentModule
    , completion = declarationNameCompletion
    , action = liftCmd . doCat
    }

-- | The add module command, adds a new module to the project
addModuleCmd :: ( ViewerAction m 
                )
             => Command u' (ViewerStateT m)
addModuleCmd = Command
    { helpLine = "add module MODULE: add a new module"
    , root = "add module"
    , parser = parseArity1 "add module" "module name"
    , isAllowed = lift hasOpenedSolution
    , completion = moduleNameCompletion
    , action = liftCmd . doAddModule
    }

-- | The remove module command, removes a module from the project
removeModuleCmd :: ( ViewerAction m 
                   )
                => Command u' (ViewerStateT m)
removeModuleCmd = Command
    { helpLine = "remove module MODULE: remove a module"
    , root = "add module"
    , parser = parseArity1 "remove module" "module name"
    , isAllowed = lift hasOpenedSolution
    , completion = moduleNameCompletion
    , action = liftCmd . doRemoveModule
    }

-- | The add export command, adds an export to the currently selected module
addExportCmd :: ( ViewerAction m 
                )
             => Command u' (ViewerStateT m)
addExportCmd = Command
    { helpLine = "add export EXPORT: add an export"
    , root = "add export"
    , parser = parseArity1 "add export" "export"
    , isAllowed = hasCurrentModule
    , completion = declarationNameCompletion
    , action = liftCmd . doAddExport
    }

-- | The remove export command, removes an export from the currently selected module
removeExportCmd :: ( ViewerAction m 
                   )
                => Command u' (ViewerStateT m)
removeExportCmd = Command
    { helpLine = "remove export EXPORT: add an export"
    , root = "remove export"
    , parser = parseArity1 "remove export" "export"
    , isAllowed = hasCurrentModule
    , completion = declarationNameCompletion
    , action = liftCmd . doRemoveExport
    }

-- | The add import command, adds an import to the currently selected module
addImportCmd :: ( ViewerAction m 
                )
             => Command u' (ViewerStateT m)
addImportCmd = Command
    { helpLine = "add import IMPORT: add an import"
    , root = "add import"
    , parser = parseArity1 "add import" "import"
    , isAllowed = hasCurrentModule
    , completion = moduleNameCompletion
    , action = liftCmd . doAddImport
    }

-- | The remove import command, removes an import from the currently selected module
removeImportCmd :: ( ViewerAction m 
                   )
                => Command u' (ViewerStateT m)
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
addDeclarationCmd :: ( ViewerIOAction m
                     ) 
                  => Editor m
                  -> Command u' (ViewerStateT m)
addDeclarationCmd editor = Command
    { helpLine = "add decl: add a new declaration"
    , root = "add decl"
    , parser = parseArity0 "add decl"
    , isAllowed = hasCurrentModule
    , completion = \_ -> return $ Just []
    , action = \_ -> liftCmd $ doAddDeclaration editor
    }

-- | The remove decl command, removes a declaration from the current module
removeDeclarationCmd :: ( ViewerAction m
                        )
                     => Command u' (ViewerStateT m)
removeDeclarationCmd = Command
    { helpLine = "remove decl DECLARATION: remove a declaration"
    , root = "remove decl"
    , parser = parseArity1 "remove decl" "declaration"
    , isAllowed = hasCurrentModule
    , completion = declarationNameCompletion
    , action = liftCmd . doRemoveDeclaration
    }

-- | The edit command, opens the editor with the specified declaration
editCmd :: ( ViewerIOAction m
           ) 
        => Editor m
        -> Command u' (ViewerStateT m)
editCmd editor = Command
    { helpLine = "edit SYMBOL: edit a declaration"
    , root = "edit"
    , parser = parseArity1 "edit" "symbol name"
    , isAllowed = hasCurrentModule
    , completion = declarationNameCompletion
    , action = liftCmd . doEdit editor
    }

-- | The build command, builds the solution using the provided builder
buildCmd :: ( ViewerIOAction m
            ) 
         => Builder m
         -> Command u' (ViewerStateT m)
buildCmd builder = Command
    { helpLine = "build: Build the project"
    , root = "build"
    , parser = parseArity0 "build"
    , isAllowed = lift hasOpenedSolution
    , completion = \_ -> return $ Just []
    , action = \_ -> liftCmd $ doBuild builder
    }

-- | The run command, runs the solution using the provided runner
runCmd :: ( ViewerIOAction m
          ) 
       => Runner m
       -> Command u' (ViewerStateT m)
runCmd runner = Command
    { helpLine = "run: Run the project executable"
    , root = "run"
    , parser = parseArity0 "run"
    , isAllowed = lift hasOpenedSolution
    , completion = \_ -> return $ Just []
    , action = \_ -> liftCmd $ doRun runner
    }

-- | The tree command, displays the solution as a tree of projects, modules,
-- and declarations
treeCmd :: ( ViewerAction m 
           )
        => Command u' (ViewerStateT m) 
treeCmd = Command
    { helpLine = "tree: Display a tree of the current project's modules and declarations"
    , root = "tree"
    , parser = parseArity0 "tree"
    , isAllowed = lift hasOpenedSolution
    , completion = \_ -> return $  Just []
    , action = \_ -> liftCmd doTree
    }

-- | The search command, finds all symbols in the project which contain the search phrase
searchCmd :: ( ViewerAction m 
             )
          => Command u' (ViewerStateT m)
searchCmd = Command
    { helpLine = "search SYMBOL: Search all modules for a declaration"
    , root = "search"
    , parser = parseArity1 "search" "search string"
    , isAllowed = lift hasOpenedSolution
    , completion = \_ -> return $ Just []
    , action = liftCmd . doSearch
    }

-- | The quit command, exits the program
quitCmd :: ( ViewerAction m 
           )
        => Command u' (ViewerStateT m) 
quitCmd = Command
    { helpLine = "quit: exit the program"
    , root = "quit"
    , parser = parseArity0 "quit"
    , isAllowed = return True
    , completion = \_ -> return $  Just []
    , action = \_ -> doQuit
    }
