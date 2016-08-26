{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
module Dialogs.SearchBar
    ( SearchBar
    , make
    , SearchMode (..)
    , setSearchMode
    , searchClickedEvent
    , setCompletion
    , addSearchClickedEventAccelerator
    , setVisible
    ) where

import Control.Monad.Trans

import Graphics.UI.Gtk
import GuiHelpers
import GuiEnv
import GuiMonad

import SearchMode

data SearchBar
    = SearchBar
    { searchLabel :: Label
    , searchBox :: Entry
    , searchButton :: Button
    , searchContainer :: HBox
    }

make
    :: ( MonadIO m
       , ContainerClass self
       )
    => self
    -> GuiEnvT proxy m' p  m SearchBar
make container = makeHBoxWith container $ \hbox -> do
    searchLabel <- makeSearchLabel hbox
    searchBox <- makeSearchBox hbox
    searchButton <- makeSearchButton hbox
    return SearchBar
         { searchLabel
         , searchBox
         , searchButton
         , searchContainer = hbox
         }

makeSearchLabel :: ( MonadIO m 
                   , ContainerClass self
                   )
                => self
                -> GuiEnvT proxy m' p m Label
makeSearchLabel = makeLabel "Find"

makeSearchBox :: ( MonadIO m
                 , ContainerClass self
                 )
              => self
              -> GuiEnvT proxy m' p  m Entry
makeSearchBox container = do
    searchBox <- withGuiComponents $ flip withSearchBuffer $ liftIO . entryNewWithBuffer
    liftIO $ container `containerAdd` searchBox
    return searchBox

makeSearchButton :: (MonadIO m)
                 => HBox -> GuiEnvT proxy m' p  m Button
makeSearchButton = makeButton "Go"

searchClickedEvent :: (Monad m) => GuiEnvSignal proxy m' p  m SearchBar Button IO ()
searchClickedEvent = searchButton `mkGuiEnvSignal` buttonActivated

setSearchMode :: SearchBar -> SearchMode -> IO ()
setSearchMode bar Find = do
    set (searchLabel bar) [labelText := "Find"]
setSearchMode bar Navigate = do
    set (searchLabel bar) [labelText := "Navigate"]
    
setCompletion :: SearchBar -> EntryCompletion -> IO ()
setCompletion bar comp = entrySetCompletion (searchBox bar) comp

addSearchClickedEventAccelerator = searchButton `addAccel` "activate"

setVisible :: SearchBar -> Bool -> IO ()
setVisible bar v = do
    set (searchBox bar) [widgetVisible := v]

{-    mapM_ (\x -> set x [widgetVisible := v])
        [ toWidget $ searchLabel bar
        , toWidget $ searchBox bar 
        , toWidget $ searchButton bar 
        , toWidget $ searchContainer bar
        ]
-}
