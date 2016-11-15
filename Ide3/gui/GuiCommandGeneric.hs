{-# LANGUAGE PolyKinds, ConstraintKinds, FlexibleContexts, ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
module GuiCommandGeneric where

import Data.Text

import System.Directory

import Control.Monad.Catch

import Control.Concurrent

import Control.Monad.Trans
import Control.Monad.Trans.Except
import Control.Monad.Trans.State.Strict (gets)

import Ide3.Types
import Ide3.Utils
import Ide3.NewMonad

import qualified Ide3.Declaration as Declaration
import qualified Ide3.Import as Import

import EnvironmentMonad

import Viewer
import ViewerMonad2

import GuiEnv
import GuiMonad
import SolutionTree

import GuiClass
import GuiClass.GuiEnv

import SearchMode

import GenericGuiEnv

import qualified GuiCommand.Internal as Internal
import GuiCommand.Internal ({-DialogOnErrorArg,-} GuiCommand )

import Args

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
doError e = dialogOnError () $ Internal.doError e

doNewStart :: ( GuiCommand2 t m' m)
           => t m ()
doNewStart = dialogOnError () $ Internal.doNewStart

doNewProjectStart :: ( GuiCommand2 t m' m )
           => t m ()
doNewProjectStart = dialogOnError () $ Internal.doNewProjectStart



doNew :: ( GuiCommand2 t m' m
         , GenericGuiEnv t
         , InitializerMonad m'
         , Args (ArgType m')
         , MonadIO m'
         )
      => Maybe FilePath
      -> String
      -> Maybe String
      -> t m ()
doNew maybeSolutionRoot projectName templateName 
    = dialogOnError () $ Internal.doNew maybeSolutionRoot projectName templateName 

doOpen :: ( GuiCommand2 t m' m 
          , MonadIO m'
          )
       => FilePath
       -> t m ()
doOpen path = dialogOnError () $ Internal.doOpen path

doGetDecl :: ( GuiCommand2 t m' m )
          => TreePath
          -> t m ()
doGetDecl path = dialogOnError () $ Internal.doGetDecl path

doBuild :: ( GuiCommand2 t m' m
           , MonadIO m'
           , MonadMask m'
           , BuilderMonad m'
           )
        => t m ThreadId
doBuild = dialogOnErrorConc $ Internal.doBuild

doRun :: ( GuiCommand2 t m' m
         , MonadMask m'
         , MonadIO m'
         , RunnerMonad m'
         )
      => t m ()
doRun = dialogOnError () $ Internal.doRun


doSave :: ( GuiCommand2 t m' m
          , MonadMask m'
          )
        => t m ()
doSave = dialogOnError () $ Internal.doSave
                

doSaveSolution :: ( GuiCommand2 t m' m
                  , MonadMask m'
                  )
              => Maybe FilePath
              -> t m ()
doSaveSolution path = dialogOnError () $ Internal.doSaveSolution path

doAddSolution :: ( GuiCommand2 t m' m
                 , m' ~ ClassSolutionInitializerMonad (t m')
                 , Args (ArgType m')
                 )
              => t m ()
doAddSolution = dialogOnError () $ Internal.doAddSolution 

doAddProject :: ( GuiCommand2 t m' m
                 , m' ~ ClassProjectInitializerMonad (t m')
                 , Args (ProjectArgType m')
                 )
              => t m ()
doAddProject = dialogOnError () $ Internal.doAddProject

doAddModule :: ( GuiCommand2 t m' m)
            => ProjectInfo
            -> ModuleInfo
            -> t m ()
doAddModule pi mi = dialogOnError () $ Internal.doAddModule pi mi

doRemoveModule :: ( GuiCommand2 t m' m )
               => ProjectInfo
               -> ModuleInfo
               -> t m ()
doRemoveModule pi mi = dialogOnError () $ Internal.doRemoveModule pi mi


doAddDeclaration :: ( GuiCommand2 t m' m
                    )
                 => ProjectInfo
                 -> ModuleInfo
                 -> DeclarationInfo
                 -> t m ()
doAddDeclaration pi mi di = dialogOnError () $ Internal.doAddDeclaration pi mi di

doRemoveDeclaration :: ( GuiCommand2 t m' m
                       )
                    => ProjectInfo
                    -> ModuleInfo
                    -> DeclarationInfo
                    -> t m ()
doRemoveDeclaration pi mi di = dialogOnError () $ Internal.doRemoveDeclaration pi mi di

doUnExportDeclaration :: ( GuiCommand2 t m' m
                         )
                      => ProjectInfo
                      -> ModuleInfo
                      -> DeclarationInfo
                      -> t m ()
doUnExportDeclaration pi mi di = dialogOnError () $ Internal.doUnExportDeclaration pi mi di

doAddImport :: ( GuiCommand2 t m' m )
            => ProjectInfo
            -> ModuleInfo
            -> String
            -> t m (Maybe (SolutionError UserError))
doAddImport pi mi importStr = dialogOnError Nothing $ Internal.doAddImport pi mi importStr

doRemoveImport :: ( GuiCommand2 t m' m )
               => ProjectInfo
               -> ModuleInfo
               -> ImportId
               -> t m ()
doRemoveImport pi mi ii = dialogOnError () $ Internal.doRemoveImport pi mi ii

doGetImport :: ( GuiCommand2 t m' m )
            => ProjectInfo
            -> ModuleInfo
            -> ImportId
            -> t m (Maybe String)
doGetImport pi mi ii = dialogOnError Nothing $ Internal.doGetImport pi mi ii

doEditImport :: ( GuiCommand2 t m' m )
             => ProjectInfo
             -> ModuleInfo
             -> ImportId
             -> String
             -> t m (Maybe (SolutionError UserError))
doEditImport pi mi ii importStr = dialogOnError Nothing $ Internal.doEditImport pi mi ii importStr

doAddExport :: ( GuiCommand2 t m' m
               )
            => ProjectInfo
            -> ModuleInfo
            -> String
            -> t m (Maybe (SolutionError UserError))
doAddExport pi mi exportStr = dialogOnError Nothing $ Internal.doAddExport pi mi exportStr

doRemoveExport :: ( GuiCommand2 t m' m
                  )
               => ProjectInfo
               -> ModuleInfo
               -> ExportId
               -> t m ()
doRemoveExport pi mi ei = dialogOnError () $ Internal.doRemoveExport pi mi ei


doGetExport :: ( GuiCommand2 t m' m
               )
            => ProjectInfo
            -> ModuleInfo
            -> ExportId
            -> t m (Maybe String)
doGetExport pi mi ei = dialogOnError Nothing $ Internal.doGetExport pi mi ei

doEditExport :: ( GuiCommand2 t m' m
                )
             => ProjectInfo
             -> ModuleInfo
             -> ExportId
             -> String
             -> t m (Maybe (SolutionError UserError))
doEditExport pi mi ei importStr = dialogOnError Nothing $ Internal.doEditExport pi mi ei importStr

doExportAll :: ( GuiCommand2 t m' m
               )
            => ProjectInfo
            -> ModuleInfo
            -> t m ()
doExportAll pi mi = dialogOnError () $ Internal.doExportAll pi mi

doSearch :: ( GuiCommand2 t m' m
            )
         => t m ()
doSearch = dialogOnError () $ Internal.doSearch

doSetSearchMode :: ( GuiCommand2 t m' m
                   )
                => SearchMode
                -> t m ()
doSetSearchMode mode = dialogOnError () $ Internal.doSetSearchMode mode

doGotoDeclaration
    :: ( GuiCommand2 t m' m
       )
    => t m ()
doGotoDeclaration = dialogOnError () $ Internal.doGotoDeclaration

doBackHistory
    :: ( GuiCommand2 t m' m
       )
    => t m ()
doBackHistory = dialogOnError () $ Internal.doBackHistory

doForwardHistory
    :: ( GuiCommand2 t m' m
       )
    => t m ()
doForwardHistory = dialogOnError () $ Internal.doForwardHistory

doJumpToErrorLocation
    :: ( GuiCommand2 t m' m
       )
    => TreePath
    -> t m Bool
doJumpToErrorLocation = dialogOnError False . Internal.doJumpToErrorLocation

