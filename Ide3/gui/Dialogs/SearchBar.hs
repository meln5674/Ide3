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
make container = makeVBoxWith container $ \vbox -> do
    hbox <- hBoxNew False 0
    boxPackEnd vbox hbox False False 0
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
                   , IsBox self
                   )
                => self
                -> GuiEnvT {-proxy-} m' p m Label
makeSearchLabel container = do
    label <- labelNew (Just "Find")
    boxPackStart container label False False 0
    return label

makeSearchBox :: ( MonadIO m
                 , IsBox self
                 )
              => self
              -> GuiEnvT {-proxy-} m' p  m Entry
makeSearchBox container = do
    searchBox <- withGuiComponents $ flip withSearchBuffer $ entryNewWithBuffer
    boxPackStart container searchBox True True 0
    return searchBox

makeSearchButton :: (MonadIO m, IsBox self)
                 => self
                 -> GuiEnvT {-proxy-} m' p  m Button
makeSearchButton container = do
    button <- buttonNewWithLabel "Go"
    boxPackStart container button False False 0
    return button

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
setVisible bar v = set (searchContainer bar) [widgetVisible := v]
