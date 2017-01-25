{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLabels, OverloadedStrings #-}
module GuiClass.GuiEnv where

import Prelude hiding (length)

import Data.Text hiding (reverse, map, init, last)

import Control.Monad
import Control.Monad.Trans

import Ide3.Types

import GI.Gtk hiding (TreePath, init)
import Data.GI.Gtk.ModelView.SeqStore
import Data.GI.Gtk.ModelView.ForestStore

import GuiMonad
import GuiClass
import GuiEnv
import GuiHelpers

import GenericGuiEnv (IdleThreadTask(..))

import SyntaxHighlighter2

instance ( MonadIO m' ) => SolutionViewClass (GuiEnvT {-proxy-} m p m') where
    getElemAtSolutionTreePath [] = return SolutionElem
    getElemAtSolutionTreePath purepath 
        = withGuiComponents 
        $ \comp -> withSolutionTree comp 
        $ \tree -> withTreePath purepath
        $ \path -> forestStoreGetValue tree path
    getTreeAtSolutionTreePath purepath
        = withGuiComponents
        $ \comp -> withSolutionTree comp
        $ \tree -> withTreePath purepath
        $ \path -> forestStoreGetTree tree path
    getForestAtSolutionTreePath purepath
        = withGuiComponents
        $ \comp -> withSolutionTree comp
        $ \tree -> do
            let descend trees [] = return trees
                descend trees (ix:ixs) = do
                    result <- withTreePath (purepath ++ [ix]) $ forestStoreLookup tree
                    case result of
                        Nothing -> return trees
                        Just tree' -> descend (tree':trees) ixs
            trees <- descend [] [0..]
            return $ reverse trees
    lookupAtSolutionTreePath purepath
        = withGuiComponents
        $ \comp -> withSolutionTree comp
        $ \tree -> withTreePath purepath
        $ \path -> do
            forestStoreLookup tree path
    setSolutionTree forest'
        = withGuiComponents
        $ \comp -> withSolutionTree comp
        $ \tree -> withTreePath []
        $ \path -> addIdleTask $ IdleThreadTask $ do
            forestStoreClear tree
            forestStoreInsertForest tree path (-1) forest'
    updateSolutionTreePathTree purepath f
        = withGuiComponents
        $ \comp -> withSolutionTree comp
        $ \tree -> withTreePath purepath
        $ \wholePath -> withTreePath (init purepath)
        $ \parentPath -> addIdleTask $ IdleThreadTask $ do
            let childIndex = last purepath
            oldTree <- forestStoreGetTree tree wholePath
            let newTree = f oldTree
            void $ forestStoreRemove tree wholePath
            forestStoreInsertTree tree parentPath childIndex newTree
    updateSolutionTreePathNode purepath f
        = withGuiComponents
        $ \comp -> withSolutionTree comp
        $ \tree -> withTreePath purepath
        $ \path -> addIdleTask $ IdleThreadTask $ do
            void $ forestStoreChange tree path f
    insertSolutionTreePathNode purepath ix node
        = withGuiComponents
        $ \comp -> withSolutionTree comp
        $ \tree -> withTreePath purepath
        $ \path -> addIdleTask $ IdleThreadTask $ do
            void $ forestStoreInsert tree path (maybe (-1) id ix) node
    insertSolutionTreePathTree purepath ix tree'
        = withGuiComponents
        $ \comp -> withSolutionTree comp
        $ \tree -> withTreePath purepath
        $ \path -> addIdleTask $ IdleThreadTask $ do
            void $ forestStoreInsertTree tree path (maybe (-1) id ix) tree'
    removeSolutionTreePathNode purepath
        = withGuiComponents
        $ \comp -> withSolutionTree comp
        $ \tree -> withTreePath purepath
        $ \path -> addIdleTask $ IdleThreadTask $ do
            void $ forestStoreRemove tree path 
            


instance ( MonadIO m' ) => SearchBarClass (GuiEnvT {-proxy-} m p m') where
    getSearchBarText = withGuiComponents $ flip withSearchBuffer $ \buffer -> lift $ do
        liftIO $ get buffer entryBufferText

instance ( MonadIO m' ) => EditorBufferClass (GuiEnvT {-proxy-} m p m') where
    setEditorBufferText text 
        = withGuiComponents
        $ flip withEditorBuffer $ \buffer -> addIdleTask $ IdleThreadTask $ do 
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
        $ flip withEditorBuffer $ \buffer -> addIdleTask $ IdleThreadTask $ do
            start <- textBufferGetIterAtLineOffset buffer startRow startCol
            end <- textBufferGetIterAtLineOffset buffer endRow endCol
            if (startRow, startCol) == (endRow, endCol)
                then textBufferPlaceCursor buffer start
                else textBufferSelectRange buffer start end
      where
        startRow = fromIntegral startRow'
        startCol = fromIntegral startCol'
        endRow = fromIntegral endRow'
        endCol = fromIntegral endCol'
    getEditorBufferPositionAtIndex offset'
        = withGuiComponents 
        $ flip withEditorBuffer $ \buffer -> do
            iter <- textBufferGetIterAtOffset buffer offset
            row <- textIterGetLine iter
            col <- textIterGetLineOffset iter
            return (Row $ fromIntegral row, Column $ fromIntegral col)
      where
        offset = fromIntegral offset'
    getEditorBufferIndexAtPosition (Row row', Column col')
        = withGuiComponents
        $ flip withEditorBuffer $ \buffer -> do
            iter <- textBufferGetIterAtLineOffset buffer row col
            liftM fromIntegral $ textIterGetOffset iter
      where
        row = fromIntegral row'
        col = fromIntegral col'
    insertTextAtEditorBufferPosition (Row row', Column col') text
        = withGuiComponents 
        $ flip withEditorBuffer $ \buffer -> addIdleTask $ IdleThreadTask $ do
            iter <- textBufferGetIterAtLineOffset buffer row col
            textBufferInsert buffer iter text (fromIntegral $ length text)
      where
        row = fromIntegral row'
        col = fromIntegral col'
    
    applySyntaxHighlighting hs
        = withGuiComponents 
        $ flip withEditorBuffer $ \buffer -> addIdleTask $ IdleThreadTask $ do
            (startBounds, endBounds) <- textBufferGetBounds buffer
            textBufferRemoveAllTags buffer startBounds endBounds
            startIter <- textBufferGetStartIter buffer
            endIter <- textBufferGetEndIter buffer
            textBufferApplyTagByName buffer (pack $ show Comment) startIter endIter
            forM_ hs $ \(HighlightInst tag 
                                      (Row startRow', Column startCol')
                                      (Row endRow', Column endCol')) -> do
                let [startRow,startCol,endRow,endCol] = map fromIntegral [startRow',startCol',endRow',endCol']
                start' <- textBufferGetIterAtLineOffset buffer startRow startCol
                end' <- textBufferGetIterAtLineOffset buffer endRow endCol
                textBufferApplyTagByName buffer (pack $ show tag) start' end'

instance ( MonadIO m' ) => BuildBufferClass (GuiEnvT {-proxy-} m p m') where
    setBuildBufferText text
        = withGuiComponents 
        $ flip withBuildBuffer $ \buffer -> addIdleTask $ IdleThreadTask $ do
            set buffer [ #text := text ]
    getBuildBufferText maybeStart maybeEnd
        = withGuiComponents 
        $ flip withBuildBuffer $ \buffer -> do
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
    selectBuildBufferText (Row startRow', Column startCol') (Row endRow', Column endCol')
        = withGuiComponents 
        $ flip withEditorBuffer $ \buffer -> addIdleTask $ IdleThreadTask $ do
            let [startRow,startCol,endRow,endCol] = map fromIntegral [startRow',startCol',endRow',endCol']
            start <- textBufferGetIterAtLineOffset buffer startRow startCol
            end <- textBufferGetIterAtLineOffset buffer endRow endCol
            textBufferSelectRange buffer start end


instance ( MonadIO m') => ErrorListClass (GuiEnvT {-proxy-} m p m') where
    clearErrorList
        = withGuiComponents
        $ flip withErrorList $ \list -> addIdleTask $ IdleThreadTask $ do
            seqStoreClear list
    addErrorToList err
        = withGuiComponents
        $ flip withErrorList $ \list -> addIdleTask $ IdleThreadTask $ do
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

