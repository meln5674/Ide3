{-|
Module      : Ide3.SrcLoc.Types
Description : Source Location Types
Copyright   : (c) Andrew Melnick, 2016

License     : BSD3
Maintainer  : meln5674@kettering.edu
Stability   : experimental
Portability : POSIX
-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Ide3.SrcLoc.Types where

import Prelude hiding (span)

import Data.Tuple

-- | A row count, 0-indexed
newtype Row = Row { unRow :: Int }
  deriving (Eq, Ord, Num)

-- | A column count, 0-indexed
newtype Column = Column { unColumn :: Int }
  deriving (Eq, Ord, Num)

-- | A pair of a row and column
data SrcLoc = SrcLoc { srcRow :: Row, srcColumn :: Column }
  deriving (Show, Eq, Ord)

-- | A start and end source location
data SrcSpan = SrcSpan { spanStart :: SrcLoc, spanEnd :: SrcLoc } 
  deriving (Show, Eq)

-- | A source location with a filename
data SrcFileLoc
    = SrcFileLoc 
    { srcLocFilename :: FilePath
    , srcFileLoc :: SrcLoc
    }
  deriving (Show, Eq)

-- | A source span with a filename
data SrcFileSpan
    = SrcFileSpan 
    { srcSpanFilename :: FilePath
    , srcFileSpan :: SrcSpan
    }
  deriving (Show, Eq)

-- | No location
noLoc :: SrcLoc
noLoc = SrcLoc (-1) (-1)

-- | No span
noSpan :: SrcSpan
noSpan = SrcSpan noLoc noLoc

-- | Class of types which can be converted to a source location
class Loccable a where
    -- | Convert to a source location
    toSrcLoc :: a -> SrcLoc

-- | Class of types which can be converted to a source location and filename
class Loccable a => FileLoccable a where
    -- | Convert to a source location and filename
    toSrcFileLoc :: a -> SrcFileLoc

-- | The class of types which can be converted to a source span
class Spannable a where
    -- | Convert to a source span
    toSrcSpan :: a -> SrcSpan

-- | The class of types which can be converted to a source span and filename
class Spannable a => FileSpannable a where
    -- | Convert to a source span and filename
    toSrcFileSpan :: a -> SrcFileSpan

-- | Make a source span from the rows and columns    
mkSrcSpan :: Row -> Column -> Row -> Column -> SrcSpan
mkSrcSpan a b c d = SrcSpan (SrcLoc a b) (SrcLoc c d)

-- | Convert row number to string
instance Show Row where
    show = show . unRow

-- | Convert column number to string
instance Show Column where
    show = show . unColumn

-- | Create row by parsing an integer
instance Read Row where
    readsPrec i s = map (swap . fmap Row . swap) (readsPrec i s)

-- | Create column by parsing an integer
instance Read Column where
    readsPrec i s = map (swap . fmap Column . swap) (readsPrec i s)

-- | Merge two source spans by taking the start of the first and the end of the
-- second
mergeSrcSpan :: SrcSpan -> SrcSpan -> SrcSpan
mergeSrcSpan a b = SrcSpan (spanStart a) (spanEnd b)

-- | Same as mergeSrcSpan but with filenames
mergeSrcFileSpan :: SrcFileSpan -> SrcFileSpan -> SrcFileSpan
mergeSrcFileSpan a b = SrcFileSpan (srcSpanFilename a) 
    $ mergeSrcSpan (srcFileSpan a) (srcFileSpan b)

-- | Lift a function on source spans to source spans with filenames
mapSrcFileSpan :: (SrcSpan -> SrcSpan) -> (SrcFileSpan -> SrcFileSpan)
mapSrcFileSpan f (SrcFileSpan path span) = SrcFileSpan path $ f span

-- | Make a source span with a filename by taking the filename of the first and
-- the span of the second
mkSrcFileSpanFrom :: (FileSpannable l, Spannable l') => l -> l' -> SrcFileSpan
mkSrcFileSpanFrom l l' = SrcFileSpan
    (srcSpanFilename $ toSrcFileSpan l)
    (toSrcSpan l')

-- | Get the start of a source span
getSpanStart :: Spannable l => l -> SrcLoc
getSpanStart = spanStart . toSrcSpan

-- | Get the end of a source span
getSpanEnd :: Spannable l => l -> SrcLoc
getSpanEnd = spanEnd . toSrcSpan

-- | Make a source span by taking the difference of the rows of two source
-- spans, and the column of the first
diffRows :: SrcLoc -> SrcLoc -> SrcLoc
diffRows (SrcLoc r1 c1) (SrcLoc r2 _) = (SrcLoc (r1-r2) c1)

-- | Identity
instance Loccable SrcLoc where
    toSrcLoc = id

-- | Discard filename
instance Loccable SrcFileLoc where
    toSrcLoc = srcFileLoc

-- | Identity
instance FileLoccable SrcFileLoc where
    toSrcFileLoc = id

-- | Identity
instance Spannable SrcSpan where
    toSrcSpan = id

-- | Discard the filename
instance Spannable SrcFileSpan where
    toSrcSpan = srcFileSpan

-- | Identity
instance FileSpannable SrcFileSpan where
    toSrcFileSpan = id

