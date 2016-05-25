{-# LANGUAGE NamedFieldPuns #-}
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


makeDeclBuffer :: IO TextBuffer
makeDeclBuffer = textBufferNew Nothing

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

