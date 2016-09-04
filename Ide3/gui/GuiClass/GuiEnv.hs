{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLabels, OverloadedStrings #-}
module GuiClass.GuiEnv where

import Prelude hiding (length)

import Data.Tree

import Data.Text hiding (reverse, map)

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Reader

import Ide3.Types

import GI.Gtk hiding (TreePath)
import qualified GI.Gtk as Gtk
import GI.Gdk
import Data.GI.Gtk.ModelView.SeqStore
import Data.GI.Gtk.ModelView.ForestStore

import GuiMonad
import GuiClass
import GuiEnv
import GuiHelpers

import SyntaxHighlighter2

instance ( Monad m, MonadIO m' ) => SolutionViewClass (GuiEnvT {-proxy-} m p m') where
    getElemAtSolutionTreePath path 
        = withGuiComponents 
        $ \comp -> withSolutionTree comp 
        $ \tree -> withTreePath path
        $ \path -> forestStoreGetValue tree path
    getTreeAtSolutionTreePath path
        = withGuiComponents
        $ \comp -> withSolutionTree comp
        $ \tree -> withTreePath path
        $ \path -> forestStoreGetTree tree path
    getForestAtSolutionTreePath path
        = withGuiComponents
        $ \comp -> withSolutionTree comp
        $ \tree -> do
            let descend trees (ix:ixs) = do
                    result <- withTreePath (path ++ [ix]) $ forestStoreLookup tree
                    case result of
                        Nothing -> return trees
                        Just tree' -> descend (tree':trees) ixs
            trees <- descend [] [0..]
            return $ reverse trees
    lookupAtSolutionTreePath path
        = withGuiComponents
        $ \comp -> withSolutionTree comp
        $ \tree -> withTreePath path
        $ \path -> do
            forestStoreLookup tree path
    setSolutionTree forest'
        = withGuiComponents
        $ \comp -> withSolutionTree comp
        $ \tree -> withTreePath []
        $ \path -> do
            forestStoreClear tree
            forestStoreInsertForest tree path (-1) forest'
    updateSolutionTreePathNode path f
        = withGuiComponents
        $ \comp -> withSolutionTree comp
        $ \tree -> withTreePath path
        $ \path -> do
            void $ forestStoreChange tree path f
    insertSolutionTreePathNode path ix node
        = withGuiComponents
        $ \comp -> withSolutionTree comp
        $ \tree -> withTreePath path
        $ \path -> do
            void $ forestStoreInsert tree path (maybe (-1) id ix) node
    insertSolutionTreePathTree path ix tree'
        = withGuiComponents
        $ \comp -> withSolutionTree comp
        $ \tree -> withTreePath path
        $ \path -> do
            void $ forestStoreInsertTree tree path (maybe (-1) id ix) tree'
    removeSolutionTreePathNode path
        = withGuiComponents
        $ \comp -> withSolutionTree comp
        $ \tree -> withTreePath path
        $ \path -> do
            void $ forestStoreRemove tree path 
            


instance ( Monad m, MonadIO m' ) => SearchBarClass (GuiEnvT {-proxy-} m p m') where
    getSearchBarText = withGuiComponents $ flip withSearchBuffer $ \buffer -> lift $ do
        liftIO $ get buffer entryBufferText

instance ( Monad m, MonadIO m' ) => EditorBufferClass (GuiEnvT {-proxy-} m p m') where
    setEditorBufferText text 
        = withGuiComponents
        $ flip withEditorBuffer $ \buffer -> do 
            set buffer [ #text := text ]
    getEditorBufferText maybeStart maybeEnd
        = withGuiComponents 
        $ flip withEditorBuffer $ \buffer -> do
            start <- case maybeStart of
                Nothing -> textBufferGetStartIter buffer
                Just (Row row', Column col') -> textBufferGetIterAtLineOffset buffer row col
                  where
                    row = fromIntegral row'
                    col = fromIntegral col'
            end <- case maybeEnd of
                Nothing -> textBufferGetEndIter buffer
                Just (Row row', Column col') -> textBufferGetIterAtLineOffset buffer row col
                  where
                    row = fromIntegral row'
                    col = fromIntegral col'
            textBufferGetText buffer start end False
    getEditorBufferCursor
        = withGuiComponents 
        $ flip withEditorBuffer $ \buffer -> do
            startMark <- textBufferGetInsert buffer
            start <- textBufferGetIterAtMark buffer startMark
            startRow <- textIterGetLine start
            startCol <- textIterGetLineOffset start
            endMark <- textBufferGetInsert buffer
            end <- textBufferGetIterAtMark buffer endMark
            endRow <- textIterGetLine end
            endCol <- textIterGetLineOffset end
            return ( (Row $ fromIntegral startRow, Column $ fromIntegral startCol)
                   , (Row $ fromIntegral endRow, Column $ fromIntegral endCol)
                   )
    selectEditorBufferText (Row startRow', Column startCol') (Row endRow', Column endCol')
        = withGuiComponents 
        $ flip withEditorBuffer $ \buffer -> lift $ liftIO $ do
            start <- textBufferGetIterAtLineOffset buffer startRow startCol
            end <- textBufferGetIterAtLineOffset buffer endRow endCol
            textBufferSelectRange buffer start end
      where
        startRow = fromIntegral startRow'
        startCol = fromIntegral startCol'
        endRow = fromIntegral endRow'
        endCol = fromIntegral endCol'
    getEditorBufferPositionAtIndex offset'
        = withGuiComponents 
        $ flip withEditorBuffer $ \buffer -> lift $ liftIO $ do
            iter <- textBufferGetIterAtOffset buffer offset
            row <- textIterGetLine iter
            col <- textIterGetLineOffset iter
            return (Row $ fromIntegral row, Column $ fromIntegral col)
      where
        offset = fromIntegral offset
    getEditorBufferIndexAtPosition (Row row', Column col')
        = withGuiComponents
        $ flip withEditorBuffer $ \buffer -> lift $ liftIO $ do
            iter <- textBufferGetIterAtLineOffset buffer row col
            liftM fromIntegral $ textIterGetOffset iter
      where
        row = fromIntegral row'
        col = fromIntegral col'
    insertTextAtEditorBufferPosition (Row row', Column col') text
        = withGuiComponents 
        $ flip withEditorBuffer $ \buffer -> lift $ liftIO $ do
            iter <- textBufferGetIterAtLineOffset buffer row col
            textBufferInsert buffer iter text (fromIntegral $ length text)
      where
        row = fromIntegral row'
        col = fromIntegral col'
    
    applySyntaxHighlighting hs
        = withGuiComponents 
        $ flip withEditorBuffer $ \buffer -> lift $ liftIO $ do
            (start, end) <- textBufferGetBounds buffer
            textBufferRemoveAllTags buffer start end
            start <- textBufferGetStartIter buffer
            end <- textBufferGetEndIter buffer
            textBufferApplyTagByName buffer (pack $ show Comment) start end
            forM_ hs $ \(HighlightInst tag 
                                      (Row startRow', Column startCol')
                                      (Row endRow', Column endCol')) -> do
                let [startRow,startCol,endRow,endCol] = map fromIntegral [startRow',startCol',endRow',endCol']
                start' <- textBufferGetIterAtLineOffset buffer startRow startCol
                end' <- textBufferGetIterAtLineOffset buffer endRow endCol
                textBufferApplyTagByName buffer (pack $ show tag) start' end'
            

instance ( Monad m, MonadIO m' ) => BuildBufferClass (GuiEnvT {-proxy-} m p m') where
    setBuildBufferText text
        = withGuiComponents 
        $ flip withBuildBuffer $ \buffer -> lift $ liftIO $ do
            set buffer [ #text := text ]
    getBuildBufferText maybeStart maybeEnd
        = withGuiComponents 
        $ flip withBuildBuffer $ \buffer -> lift $ liftIO $ do
            start <- case maybeStart of
                Nothing -> textBufferGetStartIter buffer
                Just (Row row', Column col') -> textBufferGetIterAtLineOffset buffer row col
                  where
                    row = fromIntegral row'
                    col = fromIntegral col'
            end <- case maybeEnd of
                Nothing -> textBufferGetEndIter buffer
                Just (Row row', Column col') -> textBufferGetIterAtLineOffset buffer row col
                  where
                    row = fromIntegral row'
                    col = fromIntegral col'
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
            return ( (Row $ fromIntegral startRow, Column $ fromIntegral startCol)
                   , (Row $ fromIntegral endRow, Column $ fromIntegral endCol)
                   )
    selectBuildBufferText (Row startRow', Column startCol') (Row endRow', Column endCol')
        = withGuiComponents 
        $ flip withEditorBuffer $ \buffer -> lift $ liftIO $ do
            let [startRow,startCol,endRow,endCol] = map fromIntegral [startRow',startCol',endRow',endCol']
            start <- textBufferGetIterAtLineOffset buffer startRow startCol
            end <- textBufferGetIterAtLineOffset buffer endRow endCol
            textBufferSelectRange buffer start end


instance ( Monad m, MonadIO m') => ErrorListClass (GuiEnvT {-proxy-} m p m') where
    clearErrorList
        = withGuiComponents
        $ flip withErrorList $ \list -> do
            seqStoreClear list
    addErrorToList err
        = withGuiComponents
        $ flip withErrorList $ \list -> do
            void $ seqStoreAppend list err
    getErrorAtIndex ix
        = withGuiComponents
        $ flip withErrorList $ \list -> do
            seqStoreGetValue list $ fromIntegral ix

newtype GtkIO a = GtkIO { runGtkIO :: IO a }
  deriving (Functor, Applicative, Monad, MonadIO)


instance ErrorClass GtkIO where
    displayError msg = GtkIO $ do
        dialog <- new MessageDialog 
            [ #buttons := ButtonsTypeClose
            , #messageType := MessageTypeError
            , #text := msg
            ]
        _ <- dialogRun dialog
        widgetDestroy dialog

