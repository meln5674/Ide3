{-# LANGUAGE GeneralizedNewtypeDeriving, StandaloneDeriving #-}
{-# LANGUAGE OverloadedLabels, OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
module BetterTextView 
    ( BetterTextView
    , betterTextViewNew
    , betterTextViewNewWithBuffer
    , mkBTVAttr
    ) where

import qualified Data.Text as T

import Control.Monad
import Control.Monad.Trans

import Data.GI.Base.Overloading
import qualified GI.Gtk as Gtk
import qualified GI.Gdk as Gdk
import Data.GI.Base
import Data.GI.Base.Signals

import GuiHelpers hiding (on, after)

newtype BetterTextView = BetterTextView { getBetterTextView :: Gtk.TextView }
  deriving
  ( GObject
  , Gtk.IsTextView
  , Gtk.IsWidget
  )

type instance ParentTypes BetterTextView = '[Gtk.TextView]
type instance AttributeList BetterTextView = AttributeList Gtk.TextView
type instance SignalList BetterTextView = SignalList Gtk.TextView

addTabEvent :: MonadIO m => Gtk.TextView -> m SignalHandlerId
addTabEvent textView = textView `on` #keyPressEvent $ \event -> do
    keyval <- get event Gdk.eventKeyKeyval
    when (keyval == fromIntegral Gdk.KEY_Tab) $ do
        buf <- Gtk.textViewGetBuffer textView
        let spaces = "    "
            spacesLen = fromIntegral $ toInteger $ T.length spaces
        buf `Gtk.textBufferInsertAtCursor` spaces $ spacesLen
    return $ keyval == fromIntegral Gdk.KEY_Tab

betterTextViewNew :: IO BetterTextView
betterTextViewNew = do
    textView <- new Gtk.TextView [ ]
    addTabEvent textView
    return $ BetterTextView textView

--betterTextViewNewWithBuffer :: TextBufferClass buffer => buffer -> IO BetterTextView
betterTextViewNewWithBuffer buf = do
    textView <- Gtk.textViewNewWithBuffer buf
    addTabEvent textView
    return $ BetterTextView textView

mkBTVAttr :: AttrOp Gtk.TextView tag -> SubAttrOp BetterTextView Gtk.TextView tag
mkBTVAttr attr btv = (getBetterTextView btv , attr)

