{-|
Module      : BetterTextView
Description : Improvement on the GtkTextView which offers tab-related features
Copyright   : (c) Andrew Melnick, 2016

License     : BSD3
Maintainer  : meln5674@kettering.edu
Stability   : experimental
Portability : POSIX

-}

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
    , mkBTVSignal
    ) where

import qualified Data.Text as T

import Control.Monad
import Control.Monad.Trans

import Data.GI.Base.Overloading
import GI.Gtk
import GI.Gdk
import Data.GI.Base.Signals

import GuiHelpers hiding (on, after)

-- | A wrapper around a GtkTextView which adds the following behaviors:
-- - If the user presses the tab key while selecting no text, inserts 4 spaces
--   instead of a tab character
-- - If the user presses the tab key while selecting some number of lines,
--   inserts 4 spaces at the beginning of each of those lines
-- - If the user presses Shift+tab while selecting some number of lines,
--   deletes the first 4 characters of those lines if they are spaces
newtype BetterTextView = BetterTextView { getBetterTextView :: TextView }
  deriving
  ( GObject
  , IsTextView
  , IsWidget
  )

type instance ParentTypes BetterTextView = '[TextView]
type instance AttributeList BetterTextView = AttributeList TextView
type instance SignalList BetterTextView = SignalList TextView

-- | Adds the necessary signal handler to the underlying GtkTextView to add
-- tab-related behaviors
addTabEvent :: MonadIO m => TextView -> m SignalHandlerId
addTabEvent textView = textView `on` #keyPressEvent $ \event -> do
    keyval <- liftM fromIntegral $ get event #keyval
    --liftIO $ print keyval
    buf <- textViewGetBuffer textView
    let spaces = "    "
        spacesLen = fromIntegral $ toInteger $ T.length spaces
    (selectionFlag,startIter,endIter) <- textBufferGetSelectionBounds buf
    startLine <- textIterGetLine startIter
    startColumn <- textIterGetLineOffset startIter
    endLine <- textIterGetLine endIter
    endColumn <- textIterGetLineOffset endIter
    case keyval of
        KEY_Tab -> do
            if selectionFlag
                then do
                    if startLine == endLine && startColumn == endColumn
                        then do
                            textBufferInsertAtCursor buf spaces $ spacesLen
                        else do
                            forM_ [startLine..endLine] $ \line -> do
                                insertIter <- textBufferGetIterAtLine buf line
                                textBufferInsert buf insertIter spaces spacesLen
                else textBufferInsertAtCursor buf spaces spacesLen
            return True
        KEY_ISO_Left_Tab -> do
            if selectionFlag
                then do
                    forM_ [startLine..endLine] $ \line -> do
                        removeStartIter <- textBufferGetIterAtLine buf line
                        removeEndIter <- textIterCopy removeStartIter
                        moveSuccessful <- textIterForwardChars removeStartIter 4
                        removeEndLine <- textIterGetLine removeEndIter
                        toBeRemoved <- textBufferGetText 
                                            buf removeStartIter removeEndIter True
                        let shouldRemove = moveSuccessful 
                                && removeEndLine == line 
                                && toBeRemoved == spaces
                        when shouldRemove $ do
                            textBufferDelete buf removeStartIter removeEndIter
                else return ()
            return True
        _ -> return False

-- | Create a new BetterTextView with its own buffer
betterTextViewNew :: MonadIO m => m BetterTextView
betterTextViewNew = do
    textView <- new TextView []
    void $ addTabEvent textView
    return $ BetterTextView textView

-- | Create a new BetterTextView with the provided buffer
betterTextViewNewWithBuffer :: (MonadIO m, IsTextBuffer buffer) 
                            => buffer 
                            -> m BetterTextView
betterTextViewNewWithBuffer buf = do
    textView <- textViewNewWithBuffer buf
    void $ addTabEvent textView
    return $ BetterTextView textView

-- | Modify a GI attribute meant for a GtkTextView to be applied to a 
-- BetterTextView
mkBTVAttr :: AttrOp TextView tag -> SubAttrOp BetterTextView TextView tag
mkBTVAttr attr btv = (getBetterTextView btv , attr)

mkBTVSignal :: SignalProxy TextView tag -> SubSignalProxy BetterTextView TextView tag
mkBTVSignal signal btv = (getBetterTextView btv, signal)
