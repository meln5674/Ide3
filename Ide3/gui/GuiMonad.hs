{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
module GuiMonad 
    ( GuiComponents
    , withSolutionTree
    , withEditorBuffer
    , withBuildBuffer
    , withSearchBuffer
    , withErrorList
    , initializeComponents
    , applyDeclBufferAttrs
    , defaultTextAttrs
    ) where

import Data.Text

import Control.Monad
import Control.Monad.Trans

import Data.GI.Base.Attributes
import GI.Gtk

import Data.GI.Gtk.ModelView.ForestStore
import Data.GI.Gtk.ModelView.SeqStore

import ErrorParser

import SyntaxHighlighter2

import GuiClass

import DeclarationPath



data GuiComponents
    = GuiComponents
    { projectTree :: ForestStore SolutionTreeElem
    , editorBuffer :: TextBuffer
    , buildBuffer :: TextBuffer
    , searchBuffer :: EntryBuffer
    , errorList :: SeqStore (Error ItemPath Text)
    }


defaultTextAttrs :: SyntaxComponent -> [AttrOp TextTag 'AttrSet]
defaultTextAttrs VarId = [textTagForeground := "navy", textTagWeight := 600]
defaultTextAttrs ConId = [textTagForeground := "purple4", textTagWeight := 800]
defaultTextAttrs VarSym = [textTagForeground := "grey"]
defaultTextAttrs Keyword = [textTagForeground := "blue1", textTagWeight := 600]
defaultTextAttrs Pragma = [textTagForeground := "red1"]
defaultTextAttrs Literal = [textTagForeground := "lime green"]
defaultTextAttrs Comment = [textTagForeground := "forest green", textTagWeight := 400]
defaultTextAttrs _ = [textTagForeground := "black"]

makeDeclBuffer :: MonadIO m => m TextBuffer
makeDeclBuffer = do
    buffer <- textBufferNew noTextTagTable
    table <- textBufferGetTagTable buffer
    forM_ allSyntaxComponents $ \h -> do
        let name = (pack . show) h
        tag <- textTagNew (Just name)
        table `textTagTableAdd` tag 
    return buffer

applyDeclBufferAttrs :: MonadIO m 
                     => (SyntaxComponent -> [AttrOp TextTag 'AttrSet]) 
                     -> GuiComponents
                     -> m ()
applyDeclBufferAttrs attrs comp = withEditorBuffer comp $ \buffer -> do
    table <- textBufferGetTagTable buffer
    forM_ allSyntaxComponents $ \h -> do
        let name = (pack . show) h
        let attr = attrs h
        Just tag <- table `textTagTableLookup` name
        tag `set` attr
        

makeTreeStore :: MonadIO m => m (ForestStore SolutionTreeElem)
makeTreeStore = forestStoreNew []

makeBuildBuffer :: MonadIO m => m TextBuffer
makeBuildBuffer = textBufferNew (Nothing :: Maybe TextTagTable)

makeSearchBuffer :: MonadIO m => m EntryBuffer
makeSearchBuffer = entryBufferNew (Nothing :: Maybe Text) 0

makeErrorList :: MonadIO m => m (SeqStore (Error ItemPath Text))
makeErrorList = seqStoreNew []

initializeComponents :: (MonadIO m)
                     => m GuiComponents
initializeComponents = do
    projectTree <- makeTreeStore
    editorBuffer <- makeDeclBuffer
    buildBuffer <- makeBuildBuffer
    searchBuffer <- makeSearchBuffer
    errorList <- makeErrorList
    return GuiComponents
           { projectTree
           , editorBuffer
           , buildBuffer
           , searchBuffer
           , errorList
           }
    

withSolutionTree :: GuiComponents
                 -> (ForestStore SolutionTreeElem -> a)
                 -> a
withSolutionTree comp f = f (projectTree comp)

withEditorBuffer :: GuiComponents
                 -> (TextBuffer -> a)
                 -> a
withEditorBuffer comp f = f (editorBuffer comp)

withBuildBuffer :: GuiComponents
                -> (TextBuffer -> a)
                -> a
withBuildBuffer comp f = f (buildBuffer comp)

withSearchBuffer :: GuiComponents
                 -> (EntryBuffer -> a)
                 -> a
withSearchBuffer comp f = f (searchBuffer comp)

withErrorList :: GuiComponents
               -> (SeqStore (Error ItemPath Text) -> a)
               -> a
withErrorList comp f = f (errorList comp)

{-
setDeclBufferText :: GuiComponents
                  -> String
                  -> IO ()
setDeclBufferText comp text = withEditorBuffer comp $ \buffer -> do
    start <- textBufferGetStartIter buffer
    end <- textBufferGetEndIter buffer
    textBufferRemoveAllTags buffer start end
    buffer `textBufferSetText` text
    start <- textBufferGetStartIter buffer
    end <- textBufferGetEndIter buffer
    textBufferApplyTagByName buffer (pack $ show Comment) start end
    maybeHs <- runExceptT $ getHighlights text (textBufferGetIterAtLineOffset buffer)
    case maybeHs of
        Right hs -> do
            forM_ hs $ \(HighlightInst tag start' end') -> do
                textBufferApplyTagByName buffer (pack $ show tag) start' end'
        Left _ -> return ()
-}

{-
updateDeclBufferText :: GuiComponents -> IO ()
updateDeclBufferText comp = withEditorBuffer comp $ \buffer -> do
    start <- textBufferGetStartIter buffer
    end <- textBufferGetEndIter buffer
    text <- textBufferGetText buffer start end False
    maybeHs <- runExceptT $ getHighlights text (textBufferGetIterAtLineOffset buffer)
    case maybeHs of
        Right hs -> do
            textBufferRemoveAllTags buffer start end
            start <- textBufferGetStartIter buffer
            end <- textBufferGetEndIter buffer
            textBufferApplyTagByName buffer (pack $ show Comment) start end
            forM_ hs $ \(HighlightInst tag start' end') -> do
                textBufferApplyTagByName buffer (pack $ show tag) start' end'
        Left _ -> return ()
-}

