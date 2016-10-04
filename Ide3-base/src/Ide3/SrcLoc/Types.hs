{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Ide3.SrcLoc.Types where

import Prelude hiding (span)

newtype Row = Row { unRow :: Int }
  deriving (Eq, Ord, Num)
newtype Column = Column { unColumn :: Int }
  deriving (Eq, Ord, Num)

data SrcLoc = SrcLoc { srcRow :: Row, srcColumn :: Column }
  deriving (Show, Eq, Ord)
data SrcSpan = SrcSpan { spanStart :: SrcLoc, spanEnd :: SrcLoc } 
  deriving (Show, Eq)

data SrcFileLoc = SrcFileLoc { srcLocFilename :: FilePath, srcFileLoc :: SrcLoc }
  deriving (Show, Eq)

data SrcFileSpan = SrcFileSpan { srcSpanFilename :: FilePath, srcFileSpan :: SrcSpan }
  deriving (Show, Eq)

noLoc :: SrcLoc
noLoc = SrcLoc (-1) (-1)

noSpan :: SrcSpan
noSpan = SrcSpan noLoc noLoc

class Loccable a where
    toSrcLoc :: a -> SrcLoc

class Loccable a => FileLoccable a where
    toSrcFileLoc :: a -> SrcFileLoc

-- | The class of types which can be used to retreive a substring
class Spannable a where
    -- | Convert to the SrcSpan type
    toSrcSpan :: a -> SrcSpan

class Spannable a => FileSpannable a where
    toSrcFileSpan :: a -> SrcFileSpan
    
mkSrcSpan :: Row -> Column -> Row -> Column -> SrcSpan
mkSrcSpan a b c d = SrcSpan (SrcLoc a b) (SrcLoc c d)

instance Show Row where
    show = show . unRow

instance Show Column where
    show = show . unColumn

instance Read Row where
    readsPrec i s = flip map (readsPrec i s) $ \(a,s) -> (Row a,s)

instance Read Column where
    readsPrec i s = flip map (readsPrec i s) $ \(a,s) -> (Column a,s)

mergeSrcSpan :: SrcSpan -> SrcSpan -> SrcSpan
mergeSrcSpan a b = SrcSpan (spanStart a) (spanEnd b)

mergeSrcFileSpan :: SrcFileSpan -> SrcFileSpan -> SrcFileSpan
mergeSrcFileSpan a b = SrcFileSpan (srcSpanFilename a) $ mergeSrcSpan (srcFileSpan a) (srcFileSpan b)

mapSrcFileSpan :: (SrcSpan -> SrcSpan) -> (SrcFileSpan -> SrcFileSpan)
mapSrcFileSpan f (SrcFileSpan path span) = SrcFileSpan path $ f span

mkSrcFileSpanFrom :: (FileSpannable l, Spannable l') => l -> l' -> SrcFileSpan
mkSrcFileSpanFrom l l' = SrcFileSpan (srcSpanFilename $ toSrcFileSpan l) (toSrcSpan l')

getSpanStart :: Spannable l => l -> SrcLoc
getSpanStart = spanStart . toSrcSpan

getSpanEnd :: Spannable l => l -> SrcLoc
getSpanEnd = spanEnd . toSrcSpan

diffRows :: SrcLoc -> SrcLoc -> SrcLoc
diffRows (SrcLoc r1 c1) (SrcLoc r2 c2) = (SrcLoc (r1-r2) c1)

instance Loccable SrcLoc where
    toSrcLoc = id

instance Loccable SrcFileLoc where
    toSrcLoc = srcFileLoc

instance FileLoccable SrcFileLoc where
    toSrcFileLoc = id

-- | 
instance Spannable SrcSpan where
    toSrcSpan = id

instance Spannable SrcFileSpan where
    toSrcSpan = srcFileSpan

instance FileSpannable SrcFileSpan where
    toSrcFileSpan = id

