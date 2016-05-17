module GuiMonad 
    ( GuiComponents
    , withProjectTree
    , withEditorBuffer
    , withBuildBuffer
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
    , buildBuffer :: buffer
    }



makeDeclBuffer = textBufferNew Nothing
makeTreeStore = treeStoreNew ([] :: [Tree ProjectTreeElem])
makeBuildBuffer = textBufferNew Nothing

initializeComponents :: (MonadIO m)
                     => m (GuiComponents TextBuffer)
initializeComponents = liftIO $ do
    treeStore <- makeTreeStore
    editorBuffer <- makeDeclBuffer
    buildBuffer <- makeBuildBuffer
    return $ GuiComponents treeStore editorBuffer buildBuffer
    

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

