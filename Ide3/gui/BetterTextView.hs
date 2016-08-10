{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module BetterTextView 
    ( BetterTextView
    , betterTextViewNew
    , betterTextViewNewWithBuffer
    ) where

import Data.Text

import Control.Monad
import Control.Monad.Trans

import Graphics.UI.Gtk

newtype BetterTextView = BetterTextView { getBetterTextView :: TextView }
  deriving
    ( Eq
    , Ord
    , ContainerClass
    , GObjectClass
    , WidgetClass
    , ObjectClass
    , TextViewClass
    )

addTabEvent :: TextView -> IO (ConnectId TextView)
addTabEvent textView = textView `on` keyPressEvent $ do
    keyval <- eventKeyVal
    tabKeyval <- lift $ keyvalFromName $ pack "Tab"
    lift $ when (keyval == tabKeyval) $ do
        buf <- textViewGetBuffer textView
        buf `textBufferInsertAtCursor` "    "
    return $ keyval == tabKeyval

betterTextViewNew :: IO BetterTextView
betterTextViewNew = do
    textView <- textViewNew
    addTabEvent textView
    return $ BetterTextView textView

betterTextViewNewWithBuffer :: TextBufferClass buffer => buffer -> IO BetterTextView
betterTextViewNewWithBuffer buf = do
    textView <- textViewNewWithBuffer buf
    addTabEvent textView
    return $ BetterTextView textView

