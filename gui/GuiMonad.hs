module GuiMonad 
    ( GuiComponents
    , withProjectTree
    , withEditorBuffer
    , initializeComponents
    ) where

import Data.Tree

import Control.Monad.Trans

import Graphics.UI.Gtk

import ProjectTree

data GuiComponents buffer
    = GuiComponents
    { projectTree :: TreeStore ProjectTreeElem
    , editorBuffer :: buffer
    }



makeDeclBuffer = textBufferNew Nothing
makeTreeStore = treeStoreNew ([] :: [Tree ProjectTreeElem])

initializeComponents :: (MonadIO m)
                     => m (GuiComponents TextBuffer)
initializeComponents = liftIO $ do
    buffer <- makeDeclBuffer
    treeStore <- makeTreeStore
    return $ GuiComponents treeStore buffer
    

withProjectTree :: (TextBufferClass buffer)
                => GuiComponents buffer 
                -> (TreeStore ProjectTreeElem -> m a)
                -> m a
withProjectTree comp f = f (projectTree comp)

withEditorBuffer :: (TextBufferClass buffer)
                 => GuiComponents buffer
                 -> (buffer -> m a)
                 -> m a
withEditorBuffer comp f = f (editorBuffer comp)
