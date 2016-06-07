{-# LANGUAGE NamedFieldPuns #-}
module GuiMonad 
    ( GuiComponents
    , withProjectTree
    , withEditorBuffer
    , withBuildBuffer
    , initializeComponents
    , applyDeclBufferAttrs
    , defaultTextAttrs
    , setDeclBufferText
    ) where

import Data.Text

import Data.Tree

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Except

import Graphics.UI.Gtk

import ProjectTree

import SyntaxHighlighter2

data GuiComponents buffer
    = GuiComponents
    { projectTree :: TreeStore ProjectTreeElem
    , editorBuffer :: buffer
    , buildBuffer :: buffer
    }


defaultTextAttrs :: SyntaxComponent -> [AttrOp TextTag]
defaultTextAttrs VarId = [textTagForeground := "navy"]
defaultTextAttrs ConId = [textTagForeground := "purple4"]
--defaultTextAttrs VarSym = [textTagForeground := ???]
defaultTextAttrs Keyword = [textTagForeground := "blue1"]
defaultTextAttrs Pragma = [textTagForeground := "red1"]
defaultTextAttrs Literal = [textTagForeground := "LimeGreen"]
defaultTextAttrs _ = []

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
        

makeTreeStore :: IO (TreeStore ProjectTreeElem)
makeTreeStore = treeStoreNew ([] :: [Tree ProjectTreeElem])

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
    

withProjectTree :: (TextBufferClass buffer)
                => GuiComponents buffer 
                -> (TreeStore ProjectTreeElem -> a)
                -> a
withProjectTree comp f = f (projectTree comp)

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
    textBufferRemoveAllTags buffer start end
    text <- textBufferGetText buffer start end False
    maybeHs <- runExceptT $ getHighlights text (textBufferGetIterAtLineOffset buffer)
    case maybeHs of
        Right hs -> do
            forM_ hs $ \(HighlightInst tag start' end') -> do
                textBufferApplyTagByName buffer (pack $ show tag) start' end'
        Left _ -> return ()

