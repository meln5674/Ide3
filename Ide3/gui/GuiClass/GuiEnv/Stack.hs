{-# LANGUAGE TypeFamilies #-}
module GuiClass.GuiEnv.Stack where

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

import qualified Dialogs.NewSolutionDialog as NewSolutionDialog

instance (Monad m, MonadIO m') => ProjectInitializerClass (GuiEnvT {-proxy-} m p m') where
    type ClassProjectInitializerMonad (GuiEnvT {-proxy-} m p m') = m'
    setupProjectCreator onConfirm
        = id {- access the project creator dialog #-}
        $ lift $ liftIO $ do
            {- attach `onConfirm` to the confirm clicked event of the project creator dialog -}
            return ()
            
    getProjectCreatorArg
        = withGuiComponents
        $ const {- access some kind of buffer #-}
        $ lift $ liftIO $ do
            {- get the contents of the buffer, create a stack args -}
            return $ Left $ Unsupported "Project creation"
        
        
    finalizeProjectCreator 
        = id {- access the project creator dialog -}
        $ lift $ liftIO $ do
            {- close the dialog -}
            return ()

instance ( MonadIO m
         , ViewerMonad m
         , ArgType m' ~ StackInitializerArgs
         ) => SolutionInitializerClass (GuiEnvT m' p m) where
    type ClassSolutionInitializerMonad (GuiEnvT {-proxy-} m' p m) = m'
    setupSolutionCreator
        = withNewSolutionDialog
        $ \dialog -> NewSolutionDialog.setVisible True dialog
    getSolutionCreatorArg
        = withNewSolutionDialog
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
        
        
    finalizeSolutionCreator 
        = withNewSolutionDialog
        $ \dialog -> NewSolutionDialog.setVisible False dialog


