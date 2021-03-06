{-# LANGUAGE PolyKinds, ConstraintKinds, FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
module GuiCommandGeneric where

import Data.Text (Text)

import Control.Concurrent

import Control.Monad.Trans

import Ide3.Types

import EnvironmentMonad

import GuiClass
import GuiClass.GuiEnv()

import DeclarationPath

import SearchMode

import GenericGuiEnv

import qualified GuiCommand.Internal as Internal
import GuiCommand.Internal ({-DialogOnErrorArg,-} GuiCommand )

type GuiCommand2 t m' m = 
    ( GuiCommand t m'
    , GenericGuiEnv t
    , MonadType t ~ m'
    , MonadConstraint t m'
    , NewMonadConstraint t m
    )

               
doError :: ( GuiCommand2 t m' m )
        => SolutionError UserError
        -> t m ()
doError = dialogOnError' () . Internal.doError

doNewStart :: ( GuiCommand2 t m' m)
           => t m ()
doNewStart = dialogOnError' () Internal.doNewStart

doEditSolutionStart :: ( GuiCommand2 t m' m 
                       , m' ~ ClassSolutionEditorMonad (t m')
                       , Args (SolutionEditArgType m')
                       )
                    => t m ()
doEditSolutionStart = dialogOnError' () Internal.doEditSolutionStart

doNewProjectStart :: ( GuiCommand2 t m' m )
           => t m ()
doNewProjectStart = dialogOnError' () Internal.doNewProjectStart

doEditProjectStart :: ( GuiCommand2 t m' m 
                      , m' ~ ClassProjectInitializerMonad (t m')
                      , Args (ProjectArgType m')
                      )
                   => ProjectInfo
                   -> t m ()
doEditProjectStart = dialogOnError' () . Internal.doEditProjectStart

doNew :: ( GuiCommand2 t m' m
         , Args (ArgType m')
         , MonadIO m'
         )
      => Maybe FilePath
      -> String
      -> Maybe String
      -> t m ()
doNew maybeSolutionRoot projectName
    = dialogOnError' () . Internal.doNew maybeSolutionRoot projectName

doEditSolution :: ( GuiCommand2 t m' m
                  , m' ~ ClassSolutionEditorMonad (t m')
                  , Args (SolutionEditArgType m')
                  )
               => t m ()
doEditSolution = dialogOnError' () Internal.doEditSolution
               
doOpen :: ( GuiCommand2 t m' m 
          )
       => FilePath
       -> t m ()
doOpen = dialogOnError' () . Internal.doOpen

doGetDecl :: ( GuiCommand2 t m' m )
          => TreePath
          -> t m ()
doGetDecl = dialogOnError' () . Internal.doGetDecl

doOpenItem :: ( GuiCommand2 t m' m )
          => SolutionPath
          -> t m ()
doOpenItem = dialogOnError' () . Internal.doOpenItem

doGotoSrcLoc :: ( GuiCommand2 t m' m )
             => SrcLoc
             -> t m ()
doGotoSrcLoc = dialogOnError' () . Internal.doGotoSrcLoc

doBuild :: ( GuiCommand2 t m' m
           )
        => t m ThreadId
doBuild = dialogOnErrorConc (setBuildEnabled True) Internal.doBuild

doRun :: ( GuiCommand2 t m' m
         )
      => [String]
      -> t m ()
doRun = dialogOnError' () . Internal.doRun


doSave :: ( GuiCommand2 t m' m
          )
        => t m ()
doSave = dialogOnError' () Internal.doSave
                

doSaveSolution :: ( GuiCommand2 t m' m
                  )
              => Maybe FilePath
              -> t m ()
doSaveSolution = dialogOnError' () . Internal.doSaveSolution

doAddSolution :: ( GuiCommand2 t m' m
                 , m' ~ ClassSolutionInitializerMonad (t m')
                 , Args (ArgType m')
                 )
              => t m ()
doAddSolution = dialogOnError' () Internal.doAddSolution 

doAddProject :: ( GuiCommand2 t m' m
                 , m' ~ ClassProjectInitializerMonad (t m')
                 , Args (ProjectArgType m')
                 )
              => t m ()
doAddProject = dialogOnError' () Internal.doAddProject

doEditProject :: ( GuiCommand2 t m' m
                 , m' ~ ClassProjectInitializerMonad (t m')
                 , Args (ProjectArgType m')
                 )
              => ProjectInfo
              -> t m ()
doEditProject = dialogOnError' () . Internal.doEditProject

doDeleteProject :: ( GuiCommand2 t m' m
                   , Args (ProjectArgType m')
                   )
                => ProjectInfo
                -> Bool
                -> t m ()
doDeleteProject pji = dialogOnError' () . Internal.doDeleteProject pji



doAddModule :: ( GuiCommand2 t m' m)
            => ProjectInfo
            -> ModuleInfo
            -> t m ()
doAddModule pji mi = dialogOnError' () $ Internal.doAddModule pji mi

doRemoveModule :: ( GuiCommand2 t m' m )
               => ProjectInfo
               -> ModuleInfo
               -> t m ()
doRemoveModule pji mi = dialogOnError' () $ Internal.doRemoveModule pji mi


doAddDeclaration :: ( GuiCommand2 t m' m
                    )
                 => ProjectInfo
                 -> ModuleInfo
                 -> DeclarationInfo
                 -> t m ()
doAddDeclaration pji mi di = dialogOnError' () $ Internal.doAddDeclaration pji mi di

doRemoveDeclaration :: ( GuiCommand2 t m' m
                       )
                    => ProjectInfo
                    -> ModuleInfo
                    -> DeclarationInfo
                    -> t m ()
doRemoveDeclaration pji mi di = dialogOnError' () $ Internal.doRemoveDeclaration pji mi di

doUnExportDeclaration :: ( GuiCommand2 t m' m
                         )
                      => ProjectInfo
                      -> ModuleInfo
                      -> DeclarationInfo
                      -> t m ()
doUnExportDeclaration pji mi di = dialogOnError' () $ Internal.doUnExportDeclaration pji mi di

doMoveDeclaration :: ( GuiCommand2 t m' m 
                     )
                  => ProjectInfo
                  -> ModuleInfo
                  -> DeclarationInfo
                  -> ProjectInfo
                  -> ModuleInfo
                  -> t m ()
doMoveDeclaration pji mi di pji' mi' = dialogOnError' () $ Internal.doMoveDeclaration pji mi di pji' mi'

doAddPragma :: ( GuiCommand2 t m' m )
            => ProjectInfo
            -> ModuleInfo
            -> Text
            -> t m ()
doAddPragma pji mi p = dialogOnError' () $ Internal.doAddPragma pji mi p

doRemovePragma :: ( GuiCommand2 t m' m )
            => ProjectInfo
            -> ModuleInfo
            -> Text
            -> t m ()
doRemovePragma pji mi p = dialogOnError' () $ Internal.doRemovePragma pji mi p

doEditPragma :: ( GuiCommand2 t m' m )
             => ProjectInfo
             -> ModuleInfo
             -> Pragma
             -> Pragma
             -> t m ()
doEditPragma pji mi p p' = dialogOnError' () $ Internal.doEditPragma pji mi p p'

doAddImport :: ( GuiCommand2 t m' m )
            => ProjectInfo
            -> ModuleInfo
            -> Text
            -> t m (Maybe (SolutionError UserError))
doAddImport pji mi importStr = dialogOnError' Nothing $ Internal.doAddImport pji mi importStr

doRemoveImport :: ( GuiCommand2 t m' m )
               => ProjectInfo
               -> ModuleInfo
               -> ImportId
               -> t m ()
doRemoveImport pji mi ii = dialogOnError' () $ Internal.doRemoveImport pji mi ii

doGetImport :: ( GuiCommand2 t m' m )
            => ProjectInfo
            -> ModuleInfo
            -> ImportId
            -> t m (Maybe Text)
doGetImport pji mi ii = dialogOnError' Nothing $ Internal.doGetImport pji mi ii

doEditImport :: ( GuiCommand2 t m' m )
             => ProjectInfo
             -> ModuleInfo
             -> ImportId
             -> Text
             -> t m (Maybe (SolutionError UserError))
doEditImport pji mi ii importStr = dialogOnError' Nothing $ Internal.doEditImport pji mi ii importStr

doAddExport :: ( GuiCommand2 t m' m
               )
            => ProjectInfo
            -> ModuleInfo
            -> Text
            -> t m (Maybe (SolutionError UserError))
doAddExport pji mi exportStr = dialogOnError' Nothing $ Internal.doAddExport pji mi exportStr

doRemoveExport :: ( GuiCommand2 t m' m
                  )
               => ProjectInfo
               -> ModuleInfo
               -> ExportId
               -> t m ()
doRemoveExport pji mi ei = dialogOnError' () $ Internal.doRemoveExport pji mi ei


doGetExport :: ( GuiCommand2 t m' m
               )
            => ProjectInfo
            -> ModuleInfo
            -> ExportId
            -> t m (Maybe Text)
doGetExport pji mi ei = dialogOnError' Nothing $ Internal.doGetExport pji mi ei

doEditExport :: ( GuiCommand2 t m' m
                )
             => ProjectInfo
             -> ModuleInfo
             -> ExportId
             -> Text
             -> t m (Maybe (SolutionError UserError))
doEditExport pji mi ei importStr = dialogOnError' Nothing $ Internal.doEditExport pji mi ei importStr

doExportAll :: ( GuiCommand2 t m' m
               )
            => ProjectInfo
            -> ModuleInfo
            -> t m ()
doExportAll pji mi = dialogOnError' () $ Internal.doExportAll pji mi

doSearch :: ( GuiCommand2 t m' m
            )
         => t m ()
doSearch = dialogOnError' () Internal.doSearch

doSetSearchMode :: ( GuiCommand2 t m' m
                   )
                => SearchMode
                -> t m ()
doSetSearchMode = dialogOnError' () . Internal.doSetSearchMode

doGotoDeclaration
    :: ( GuiCommand2 t m' m
       )
    => t m ()
doGotoDeclaration = dialogOnError' () Internal.doGotoDeclaration

doBackHistory
    :: ( GuiCommand2 t m' m
       )
    => t m ()
doBackHistory = dialogOnError' () Internal.doBackHistory

doForwardHistory
    :: ( GuiCommand2 t m' m
       )
    => t m ()
doForwardHistory = dialogOnError' () Internal.doForwardHistory

doJumpToErrorLocation
    :: ( GuiCommand2 t m' m
       )
    => TreePath
    -> t m Bool
doJumpToErrorLocation = dialogOnError' False . Internal.doJumpToErrorLocation

