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
import GI.Gtk
import GI.Gdk
import Data.GI.Base
import Data.GI.Base.Signals

import GuiHelpers hiding (on, after)

newtype BetterTextView = BetterTextView { getBetterTextView :: TextView }
  deriving
  ( GObject
  , IsTextView
  , IsWidget
  )

type instance ParentTypes BetterTextView = '[TextView]
type instance AttributeList BetterTextView = AttributeList TextView
type instance SignalList BetterTextView = SignalList TextView

addTabEvent :: MonadIO m => TextView -> m SignalHandlerId
addTabEvent textView = textView `on` #keyPressEvent $ \event -> do
    keyval <- liftM fromIntegral $ get event #keyval
    liftIO $ print keyval
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
            when selectionFlag $ do
                if startLine == endLine
                    then do
                        textBufferInsertAtCursor buf spaces $ spacesLen
                    else do
                        forM_ [startLine..endLine] $ \line -> do
                            insertIter <- textBufferGetIterAtLine buf line
                            textBufferInsert buf insertIter spaces spacesLen
            return True
        KEY_ISO_Left_Tab -> do
            when selectionFlag $ do
                forM_ [startLine..endLine] $ \line -> do
                    removeStartIter <- textBufferGetIterAtLine buf line
                    removeEndIter <- textIterCopy removeStartIter
                    moveSuccessful <- textIterForwardChars removeStartIter 4
                    removeEndLine <- textIterGetLine removeEndIter
                    when (moveSuccessful && removeEndLine == line) $ do
                        toBeRemoved <- textBufferGetText buf removeStartIter removeEndIter True
                        when (toBeRemoved == spaces) $ do
                            textBufferDelete buf removeStartIter removeEndIter
            return True
        _ -> return False

betterTextViewNew :: IO BetterTextView
betterTextViewNew = do
    textView <- new TextView []
    addTabEvent textView
    return $ BetterTextView textView

--betterTextViewNewWithBuffer :: TextBufferClass buffer => buffer -> IO BetterTextView
betterTextViewNewWithBuffer buf = do
    textView <- textViewNewWithBuffer buf
    addTabEvent textView
    return $ BetterTextView textView

mkBTVAttr :: AttrOp TextView tag -> SubAttrOp BetterTextView TextView tag
mkBTVAttr attr btv = (getBetterTextView btv , attr)

