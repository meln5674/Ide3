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
    --, mkBTVAttr
    --, mkBTVAttrOp
    --, mkBTVSignal
    ) where

import Data.Char

import Data.Monoid
import Data.Text (Text)
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

instance WidgetContainer BetterTextView where
    type WidgetType BetterTextView = TextView
    getWidget = getBetterTextView

type instance ParentTypes BetterTextView = '[TextView]
type instance AttributeList BetterTextView = AttributeList TextView
type instance SignalList BetterTextView = SignalList TextView

indentString :: Text
indentString = "    "

isIndentChar :: Char -> Bool
isIndentChar c = generalCategory c `elem` [Space, ParagraphSeparator] 

-- | Returns a flag indicating if text is selected, along with the iterator where
-- selection begins and ends
textBufferGetSelectionBounds' :: MonadIO m => TextBuffer -> m (Bool, TextIter, TextIter)
textBufferGetSelectionBounds' textBuffer
    = (,,)
    <$> get textBuffer #hasSelection
    <*> (textBufferGetSelectionBound textBuffer >>= textBufferGetIterAtMark textBuffer)
    <*> (textBufferGetInsert textBuffer >>= textBufferGetIterAtMark textBuffer)

textBufferGetLine :: MonadIO m => TextBuffer -> Int -> m Text
textBufferGetLine textBuffer lineNumber = do
    startIter <- textBufferGetIterAtLine textBuffer $ fromIntegral lineNumber
    endIter <- textBufferGetIterAtLine textBuffer $ fromIntegral $ lineNumber+1
    textIterBackwardChar endIter
    textBufferGetSlice textBuffer startIter endIter True

deleteSelection :: MonadIO m => TextBuffer -> m Bool
deleteSelection textBuffer = do
    (selectionFlag,startIter,endIter) <- textBufferGetSelectionBounds textBuffer
    when selectionFlag $ textBufferDelete textBuffer startIter endIter
    return selectionFlag

getSelectionLines :: MonadIO m => TextBuffer -> m (Int, Int)
getSelectionLines textBuffer = do
    (_,startIter,endIter) <- textBufferGetSelectionBounds textBuffer
    startLine <- textIterGetLine startIter
    endLine <- textIterGetLine endIter
    return (fromIntegral startLine, fromIntegral endLine)

getSelectionFirstLine :: MonadIO m => TextBuffer -> m Int
getSelectionFirstLine textBuffer = do
    (_,startIter,_) <- textBufferGetSelectionBounds textBuffer
    fromIntegral <$> textIterGetLine startIter

getSelectionLastLine :: MonadIO m => TextBuffer -> m Int
getSelectionLastLine textBuffer = do
    (_,_,endIter) <- textBufferGetSelectionBounds textBuffer
    fromIntegral <$> textIterGetLine endIter

getCursorLine :: MonadIO m => TextBuffer -> m Int
getCursorLine textBuffer = do
    (_,_,endIter) <- textBufferGetSelectionBounds' textBuffer
    fromIntegral <$> textIterGetLine endIter

addIndentToLine :: MonadIO m => TextBuffer -> Int -> m ()
addIndentToLine textBuffer lineNumber = do
    insertIter <- textBufferGetIterAtLine textBuffer $ fromIntegral lineNumber
    textBufferInsert textBuffer insertIter indentString (-1)

removeIndentFromLine :: MonadIO m => TextBuffer -> Int -> m ()
removeIndentFromLine textBuffer lineNumber = do
    lineIndent <- getLineIndent textBuffer lineNumber
    let removableIndent = T.take (T.length indentString) $ fst $ T.span isIndentChar lineIndent
    startIter <- textBufferGetIterAtLine textBuffer $ fromIntegral lineNumber
    endIter <- textBufferGetIterAtLine textBuffer $ fromIntegral lineNumber
    textIterForwardChars endIter $ fromIntegral $ T.length removableIndent
    textBufferDelete textBuffer startIter endIter

getLineIndent :: MonadIO m => TextBuffer -> Int -> m Text
getLineIndent textBuffer lineNumber = do
    lineText <- textBufferGetLine textBuffer lineNumber
    return $ fst $ T.span isIndentChar lineText

advanceCursor :: MonadIO m => TextBuffer -> Int -> m ()
advanceCursor textBuffer charCount = do
    cursorIter <- textBufferGetInsert textBuffer >>= textBufferGetIterAtMark textBuffer
    textIterForwardChars cursorIter $ fromIntegral charCount
    textBufferPlaceCursor textBuffer cursorIter

insertIndent :: MonadIO m => TextBuffer -> m ()
insertIndent textBuffer = do
    textBufferInsertAtCursor textBuffer indentString (-1)

getCurrentLineStartIter :: MonadIO m => TextBuffer -> m TextIter
getCurrentLineStartIter textBuffer = do
    currentLine <- getCursorLine textBuffer
    textBufferGetIterAtLine textBuffer $ fromIntegral currentLine

getCurrentLineIndentEndIter :: MonadIO m => TextBuffer -> m TextIter
getCurrentLineIndentEndIter textBuffer = do
    currentLine <- getCursorLine textBuffer
    lineStartIter <- textBufferGetIterAtLine textBuffer $ fromIntegral currentLine
    currentLineIndent <- getLineIndent textBuffer currentLine
    textIterForwardChars lineStartIter $ fromIntegral $ T.length currentLineIndent
    return lineStartIter
        

afterKeyReturn :: MonadIO m => TextBuffer -> m Bool
afterKeyReturn textBuffer = do
    currentLineNumber <- getCursorLine textBuffer
    let previousLineNumber = currentLineNumber - 1
    previousLineIndent <- getLineIndent textBuffer previousLineNumber
    let toInsert = previousLineIndent
    textBufferInsertAtCursor textBuffer toInsert (-1)
    liftIO $ print toInsert
    return False

onKeyTab :: MonadIO m => TextBuffer -> m Bool
onKeyTab textBuffer = do
    (selectionFlag,startIter,endIter) <- textBufferGetSelectionBounds textBuffer
    if selectionFlag
        then do
            noSelection <- textIterEqual startIter endIter
            if noSelection
                then insertIndent textBuffer
                else do
                    (startLine, endLine) <- getSelectionLines textBuffer
                    forM_ [startLine..endLine] $ addIndentToLine textBuffer
        else insertIndent textBuffer
    return True

onKeyLeftTab :: MonadIO m => TextBuffer -> m Bool
onKeyLeftTab textBuffer = do
    (selectionFlag,_,_) <- textBufferGetSelectionBounds textBuffer
    when selectionFlag $ do
        (startLine, endLine) <- getSelectionLines textBuffer
        forM_ [startLine..endLine] $ removeIndentFromLine textBuffer
    return True

onKeyHome :: MonadIO m => TextBuffer -> Bool -> m Bool
onKeyHome textBuffer shiftDown = do
    startIter <- getCurrentLineStartIter textBuffer
    afterIndentIter <- getCurrentLineIndentEndIter textBuffer
    (_,selectionBoundIter,cursorIter) <- textBufferGetSelectionBounds' textBuffer
    isAtIndentEnd <- textIterEqual afterIndentIter cursorIter
    let newCursorIter = if isAtIndentEnd then startIter else afterIndentIter
        newSelectionBoundIter = if shiftDown then selectionBoundIter else newCursorIter
    textBufferSelectRange textBuffer newCursorIter newSelectionBoundIter
    return True

-- | Adds the necessary signal handler to the underlying GtkTextView to add
-- tab-related behaviors
addTabEvent :: MonadIO m => TextView -> m (SignalHandlerId, SignalHandlerId)
addTabEvent textView = do
    onId <- textView `on` #keyPressEvent $ \event -> do
        buf <- textViewGetBuffer textView
        keyval <- fromIntegral <$> get event #keyval
        shiftDown <- (ModifierTypeShiftMask `elem`) <$> get event #state
        case keyval of
            KEY_Home -> onKeyHome buf shiftDown
            KEY_Tab -> onKeyTab buf
            KEY_ISO_Left_Tab -> onKeyLeftTab buf
            _ -> return False
    afterId <- textView `after` #keyReleaseEvent $ \event -> do
        buf <- textViewGetBuffer textView
        keyval <- fromIntegral <$> get event #keyval
        case keyval of
            KEY_Return -> afterKeyReturn buf
            _ -> return False
    return (onId, afterId)

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
