{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module GuiClass.GuiT where

import Data.Text

import System.Directory
import System.FilePath

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader

import ViewerMonad
import Viewer

import Ide3.NewMonad
import Ide3.Types
import Ide3.Utils

--import Graphics.UI.Gtk

import EnvironmentMonad

import Initializer.Stack

import GuiMonad
import GuiClass
import GuiEnv
import GuiHelpers

import SolutionTree

import Dialogs.Class

import qualified Dialogs.NewSolutionDialog as NewSolutionDialog

import GuiT


instance ( Monad m'
         , MonadIO m
         ) => ProjectInitializerClass (GuiT m' p m) where
    type ClassProjectInitializerMonad (GuiT m' p m) = m'
    setupProjectCreator onConfirm
        = id {- access the project creator dialog #-}
        $ lift $ liftIO $ do
            {- attach `onConfirm` to the confirm clicked event of the project creator dialog -}
            return ()
            
    getProjectCreatorArg
        = liftEnv $ withGuiComponents
        $ const {- access some kind of buffer #-}
        $ lift $ liftIO $ do
            {- get the contents of the buffer, create a stack args -}
            return $ Left $ Unsupported "Project creation"
        
        
    finalizeProjectCreator 
        = id {- access the project creator dialog -}
        $ lift $ liftIO $ do
            {- close the dialog -}
            return ()


getSolutionCreatorArg' :: forall m' p m u
                        . ( MonadIO m
                          , ViewerMonad m
                          , m ~ m'
                          )
                       => GuiT m' p m (Either (SolutionError u) StackInitializerArgs)
getSolutionCreatorArg'
        = liftDialogs
        $ withNewSolutionDialogM
        $ \dialog -> do
            projectRoot <- NewSolutionDialog.getSelectedFolder dialog
            projectName <- liftM unpack $ NewSolutionDialog.getSolutionName dialog
            templateName <- liftM (fmap unpack) $ NewSolutionDialog.getTemplateName dialog
            runExceptT $ case projectRoot of
                Nothing -> throwE $ InvalidOperation "Please choose a directory" ""
                Just projectRoot -> do
                    wrapIOError $ setCurrentDirectory $ projectRoot
                    bounce $ setDirectoryToOpen $ projectRoot </> projectName
                    return $ StackInitializerArgs projectName templateName
            


instance ( MonadIO m
         , ViewerMonad m
         , ArgType m' ~ StackInitializerArgs
         , m ~ m'
         ) => SolutionInitializerClass (GuiT m' p m) where
    type ClassSolutionInitializerMonad (GuiT m' p m) = m'
    setupSolutionCreator
        = liftDialogs
        $ withNewSolutionDialogM
        $ \dialog -> NewSolutionDialog.setVisible dialog True
    getSolutionCreatorArg = getSolutionCreatorArg'
        
        
    finalizeSolutionCreator 
        = liftDialogs
        $ withNewSolutionDialogM
        $ \dialog -> NewSolutionDialog.setVisible dialog False


