{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings, OverloadedLabels #-}
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

import Data.Text

import Control.Monad.Trans

import GI.Gtk hiding (SearchBar)
import GI.Gdk 

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
       , IsContainer self
       )
    => self
    -> GuiEnvT {-proxy-} m' p  m SearchBar
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
                   , IsContainer self
                   )
                => self
                -> GuiEnvT {-proxy-} m' p m Label
makeSearchLabel = makeLabel "Find"

makeSearchBox :: ( MonadIO m
                 , IsContainer self
                 )
              => self
              -> GuiEnvT {-proxy-} m' p  m Entry
makeSearchBox container = do
    searchBox <- withGuiComponents $ flip withSearchBuffer $ liftIO . entryNewWithBuffer
    liftIO $ container `containerAdd` searchBox
    return searchBox

makeSearchButton :: (MonadIO m)
                 => HBox -> GuiEnvT {-proxy-} m' p  m Button
makeSearchButton = makeButton "Go"

type SearchBarSignal object info = SubSignalProxy SearchBar object info

searchClickedEvent :: SearchBarSignal Button ButtonClickedSignalInfo
searchClickedEvent searchBar = (searchButton searchBar, #clicked)

setSearchMode :: MonadIO m => SearchBar -> SearchMode -> m ()
setSearchMode bar Find = do
    set (searchLabel bar) [#label := "Find"]
setSearchMode bar Navigate = do
    set (searchLabel bar) [#label := "Navigate"]
    
setCompletion :: MonadIO m => SearchBar -> EntryCompletion -> m ()
setCompletion bar comp = set (searchBox bar) [ #completion := comp ]


addSearchClickedEventAccelerator :: (MonadIO m, IsAccelGroup group, Integral key)
                                  => SearchBar 
                                  -> group
                                  -> key
                                  -> [ModifierType] 
                                  -> [AccelFlags]
                                  -> m ()
addSearchClickedEventAccelerator = searchButton `addAccel` "activate"

setVisible :: MonadIO m => SearchBar -> Bool -> m ()
setVisible bar v = do
    set (searchBox bar) [widgetVisible := v]
