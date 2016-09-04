{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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

import Data.Tree

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Except

import Data.GI.Base.Attributes
import GI.Gtk

import Data.GI.Gtk.ModelView.ForestStore
import Data.GI.Gtk.ModelView.SeqStore

import ErrorParser.Types

import SolutionTree

import SyntaxHighlighter2

import GuiClass

import DeclarationPath



data GuiComponents
    = GuiComponents
    { projectTree :: ForestStore SolutionTreeElem
    , editorBuffer :: TextBuffer
    , buildBuffer :: TextBuffer
    , searchBuffer :: EntryBuffer
    , errorList :: SeqStore (Error ItemPath)
    }


defaultTextAttrs :: SyntaxComponent -> [AttrOp TextTag AttrSet]
defaultTextAttrs VarId = [textTagForeground := "navy", textTagWeight := 600]
defaultTextAttrs ConId = [textTagForeground := "purple4", textTagWeight := 800]
defaultTextAttrs VarSym = [textTagForeground := "grey"]
defaultTextAttrs Keyword = [textTagForeground := "blue1", textTagWeight := 600]
defaultTextAttrs Pragma = [textTagForeground := "red1"]
defaultTextAttrs Literal = [textTagForeground := "lime green"]
defaultTextAttrs Comment = [textTagForeground := "forest green", textTagWeight := 400]
defaultTextAttrs _ = [textTagForeground := "black"]

makeDeclBuffer :: IO TextBuffer
makeDeclBuffer = do
    buffer <- textBufferNew (Nothing :: Maybe TextTagTable)
    table <- textBufferGetTagTable buffer
    forM_ allSyntaxComponents $ \h -> do
        let name = (pack . show) h
        tag <- textTagNew (Just name)
        table `textTagTableAdd` tag 
    return buffer

applyDeclBufferAttrs :: (SyntaxComponent -> [AttrOp TextTag AttrSet]) 
                     -> GuiComponents
                     -> IO ()
applyDeclBufferAttrs attrs comp = withEditorBuffer comp $ \buffer -> do
    table <- textBufferGetTagTable buffer
    forM_ allSyntaxComponents $ \h -> do
        let name = (pack . show) h
        let attr = attrs h
        Just tag <- table `textTagTableLookup` name
        tag `set` attr
        

makeTreeStore :: IO (ForestStore SolutionTreeElem)
makeTreeStore = forestStoreNew []

makeBuildBuffer :: IO TextBuffer
makeBuildBuffer = textBufferNew (Nothing :: Maybe TextTagTable)

makeSearchBuffer :: IO EntryBuffer
makeSearchBuffer = entryBufferNew (Nothing :: Maybe Text) 0

makeErrorList :: IO (SeqStore (Error ItemPath))
makeErrorList = seqStoreNew []

initializeComponents :: (MonadIO m)
                     => m GuiComponents
initializeComponents = liftIO $ do
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
               -> (SeqStore (Error ItemPath) -> a)
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

