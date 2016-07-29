{-# LANGUAGE NamedFieldPuns #-}
module GuiMonad 
    ( GuiComponents
    , withSolutionTree
    , withEditorBuffer
    , withBuildBuffer
    , initializeComponents
    , applyDeclBufferAttrs
    , defaultTextAttrs
    , setDeclBufferText
    , updateDeclBufferText
    ) where

import Data.Text

import Data.Tree

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Except

import Graphics.UI.Gtk

import SolutionTree

import SyntaxHighlighter2

data GuiComponents buffer
    = GuiComponents
    { projectTree :: TreeStore SolutionTreeElem
    , editorBuffer :: buffer
    , buildBuffer :: buffer
    }


defaultTextAttrs :: SyntaxComponent -> [AttrOp TextTag]
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
    buffer <- textBufferNew Nothing
    table <- textBufferGetTagTable buffer
    forM_ allSyntaxComponents $ \h -> do
        let name = (pack . show) h
        tag <- textTagNew (Just name)
        table `textTagTableAdd` tag 
    return buffer

applyDeclBufferAttrs :: (TextBufferClass buffer) 
                     => (SyntaxComponent -> [AttrOp TextTag]) 
                     -> GuiComponents buffer 
                     -> IO ()
applyDeclBufferAttrs attrs comp = withEditorBuffer comp $ \buffer -> do
    table <- textBufferGetTagTable buffer
    forM_ allSyntaxComponents $ \h -> do
        let name = (pack . show) h
        let attr = attrs h
        Just tag <- table `textTagTableLookup` name
        tag `set` attr
        

makeTreeStore :: IO (TreeStore SolutionTreeElem)
makeTreeStore = treeStoreNew ([] :: [Tree SolutionTreeElem])

makeBuildBuffer :: IO TextBuffer
makeBuildBuffer = textBufferNew Nothing

initializeComponents :: (MonadIO m)
                     => m (GuiComponents TextBuffer)
initializeComponents = liftIO $ do
    projectTree <- makeTreeStore
    editorBuffer <- makeDeclBuffer
    buildBuffer <- makeBuildBuffer
    return GuiComponents
           { projectTree
           , editorBuffer
           , buildBuffer
           }
    

withSolutionTree :: (TextBufferClass buffer)
                => GuiComponents buffer 
                -> (TreeStore SolutionTreeElem -> a)
                -> a
withSolutionTree comp f = f (projectTree comp)

withEditorBuffer :: (TextBufferClass buffer)
                 => GuiComponents buffer
                 -> (buffer -> a)
                 -> a
withEditorBuffer comp f = f (editorBuffer comp)

withBuildBuffer :: (TextBufferClass buffer)
                 => GuiComponents buffer
                 -> (buffer -> a)
                 -> a
withBuildBuffer comp f = f (buildBuffer comp)

setDeclBufferText :: (TextBufferClass buffer)
                  => GuiComponents buffer
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

updateDeclBufferText :: TextBufferClass self => GuiComponents self -> IO ()
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


