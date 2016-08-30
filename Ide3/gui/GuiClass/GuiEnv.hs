{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module GuiClass.GuiEnv where

import Data.Text hiding (reverse)

import Control.Monad
import Control.Monad.Trans

import Graphics.UI.Gtk

import GuiMonad
import GuiClass
import GuiEnv

import SyntaxHighlighter2

instance ( Monad m, MonadIO m' ) => SolutionViewClass (GuiEnvT proxy m p m') where
    getElemAtSolutionPath path 
        = withGuiComponents 
        $ \comp -> withSolutionTree comp 
        $ \tree -> liftIO $ treeStoreGetValue tree path
    getTreeAtSolutionPath path
        = withGuiComponents
        $ \comp -> withSolutionTree comp
        $ \tree -> liftIO $ treeStoreGetTree tree path
    getForestAtSolutionPath path
        = withGuiComponents
        $ \comp -> withSolutionTree comp
        $ \tree -> liftIO $ do
            let foo trees (ix:ixs) = do
                    result <- treeStoreLookup tree $ path ++ [ix]
                    case result of
                        Nothing -> return trees
                        Just tree' -> foo (tree':trees) ixs
            trees <- foo [] [0..]
            return $ reverse trees
    setSolutionTree forest'
        = withGuiComponents
        $ \comp -> withSolutionTree comp
        $ \tree -> do
            liftIO $ treeStoreClear tree
            liftIO $ treeStoreInsertForest tree [] (-1) forest'

instance ( Monad m, MonadIO m' ) => SearchBarClass (GuiEnvT proxy m p m') where
    getSearchBarText = withGuiComponents $ flip withSearchBuffer $ \buffer -> lift $ do
        liftIO $ get buffer entryBufferText

instance ( Monad m, MonadIO m' ) => EditorBufferClass (GuiEnvT proxy m p m') where
    setEditorBufferText text 
        = withGuiComponents
        $ flip withEditorBuffer $ \buffer -> lift $ liftIO $ do 
            textBufferSetText buffer text
    getEditorBufferText maybeStart maybeEnd
        = withGuiComponents 
        $ flip withEditorBuffer $ \buffer -> lift $ liftIO $ do
            start <- case maybeStart of
                Nothing -> textBufferGetStartIter buffer
                Just (Row row, Column col) -> textBufferGetIterAtLineOffset buffer row col
            end <- case maybeEnd of
                Nothing -> textBufferGetEndIter buffer
                Just (Row row, Column col) -> textBufferGetIterAtLineOffset buffer row col
            textBufferGetText buffer start end False
    getEditorBufferCursor
        = withGuiComponents 
        $ flip withEditorBuffer $ \buffer -> lift $ liftIO $ do
            startMark <- textBufferGetInsert buffer
            start <- textBufferGetIterAtMark buffer startMark
            startRow <- textIterGetLine start
            startCol <- textIterGetLineOffset start
            endMark <- textBufferGetInsert buffer
            end <- textBufferGetIterAtMark buffer endMark
            endRow <- textIterGetLine end
            endCol <- textIterGetLineOffset end
            return ((Row startRow, Column startCol), (Row endRow, Column endCol))
    selectEditorBufferText (Row startRow, Column startCol) (Row endRow, Column endCol)
        = withGuiComponents 
        $ flip withEditorBuffer $ \buffer -> lift $ liftIO $ do
            start <- textBufferGetIterAtLineOffset buffer startRow startCol
            end <- textBufferGetIterAtLineOffset buffer endRow endCol
            textBufferSelectRange buffer start end
    getEditorBufferPositionAtIndex offset 
        = withGuiComponents 
        $ flip withEditorBuffer $ \buffer -> lift $ liftIO $ do
            iter <- textBufferGetIterAtOffset buffer offset
            row <- textIterGetLine iter
            col <- textIterGetLineOffset iter
            return (Row row, Column col)
    getEditorBufferIndexAtPosition (Row row, Column col)
        = withGuiComponents
        $ flip withEditorBuffer $ \buffer -> lift $ liftIO $ do
            iter <- textBufferGetIterAtLineOffset buffer row col
            get iter textIterOffset
    insertTextAtEditorBufferPosition (Row row, Column col) text
        = withGuiComponents 
        $ flip withEditorBuffer $ \buffer -> lift $ liftIO $ do
            iter <- textBufferGetIterAtLineOffset buffer row col
            textBufferInsert buffer iter text
    applySyntaxHighlighting hs
        = withGuiComponents 
        $ flip withEditorBuffer $ \buffer -> lift $ liftIO $ do
            (start, end) <- textBufferGetBounds buffer
            textBufferRemoveAllTags buffer start end
            start <- textBufferGetStartIter buffer
            end <- textBufferGetEndIter buffer
            textBufferApplyTagByName buffer (pack $ show Comment) start end
            forM_ hs $ \(HighlightInst tag 
                                      (Row startRow, Column startCol)
                                      (Row endRow, Column endCol)) -> do
                start' <- textBufferGetIterAtLineOffset buffer startRow startCol
                end' <- textBufferGetIterAtLineOffset buffer endRow endCol
                textBufferApplyTagByName buffer (pack $ show tag) start' end'
            

instance ( Monad m, MonadIO m' ) => BuildBufferClass (GuiEnvT proxy m p m') where
    setBuildBufferText text
        = withGuiComponents 
        $ flip withBuildBuffer $ \buffer -> lift $ liftIO $ do
            postGUISync $ buffer `textBufferSetText` text
    getBuildBufferText maybeStart maybeEnd
        = withGuiComponents 
        $ flip withBuildBuffer $ \buffer -> lift $ liftIO $ do
            start <- case maybeStart of
                Nothing -> textBufferGetStartIter buffer
                Just (Row row, Column col) -> textBufferGetIterAtLineOffset buffer row col
            end <- case maybeEnd of
                Nothing -> textBufferGetEndIter buffer
                Just (Row row, Column col) -> textBufferGetIterAtLineOffset buffer row col
            textBufferGetText buffer start end False
    getBuildBufferCursor 
        = withGuiComponents 
        $ flip withEditorBuffer $ \buffer -> lift $ liftIO $ do
            startMark <- textBufferGetInsert buffer
            start <- textBufferGetIterAtMark buffer startMark
            startRow <- textIterGetLine start
            startCol <- textIterGetLineOffset start
            endMark <- textBufferGetInsert buffer
            end <- textBufferGetIterAtMark buffer endMark
            endRow <- textIterGetLine end
            endCol <- textIterGetLineOffset end
            return ((Row startRow, Column startCol), (Row endRow, Column endCol))
    selectBuildBufferText (Row startRow, Column startCol) (Row endRow, Column endCol)
        = withGuiComponents 
        $ flip withEditorBuffer $ \buffer -> lift $ liftIO $ do
            start <- textBufferGetIterAtLineOffset buffer startRow startCol
            end <- textBufferGetIterAtLineOffset buffer endRow endCol
            textBufferSelectRange buffer start end

instance ( Monad m, MonadIO m') => ErrorListClass (GuiEnvT proxy m p m') where
    clearErrorList
        = withGuiComponents
        $ flip withErrorList $ \list -> lift $ liftIO $ do
            postGUISync $ listStoreClear list
    addErrorToList err
        = withGuiComponents
        $ flip withErrorList $ \list -> lift $ liftIO $ do
            putStrLn $ "Adding error: " ++ show err
            postGUISync $ void $ listStoreAppend list err

newtype GtkIO a = GtkIO { runGtkIO :: IO a }
  deriving (Functor, Applicative, Monad, MonadIO)

instance ErrorClass GtkIO where
    displayError msg = GtkIO $ postGUISync $ do
        dialog <- messageDialogNew
            Nothing
            []
            MessageError
            ButtonsClose
            msg
        _ <- dialogRun dialog
        widgetDestroy dialog

