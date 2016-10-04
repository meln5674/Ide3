module Ide3.SrcLoc.Exts where

import Prelude hiding (span)

import qualified Language.Haskell.Exts.SrcLoc as Exts
import qualified Language.Haskell.Exts.Comments as Exts

import Ide3.SrcLoc.Types



instance Loccable Exts.SrcLoc where
    toSrcLoc (Exts.SrcLoc _ a b) = SrcLoc (Row a) (Column b)

instance FileLoccable Exts.SrcLoc where
    toSrcFileLoc loc = SrcFileLoc (Exts.srcFilename loc) $ toSrcLoc loc



instance Spannable Exts.SrcSpan where
    toSrcSpan (Exts.SrcSpan _ a b c d) = mkSrcSpan (Row a) (Column b) (Row c) (Column d)

instance FileSpannable Exts.SrcSpan where
    toSrcFileSpan span = SrcFileSpan (Exts.srcSpanFilename span) $ toSrcSpan span

-- | 
instance Spannable Exts.SrcSpanInfo where
    toSrcSpan = toSrcSpan . Exts.srcInfoSpan

instance FileSpannable Exts.SrcSpanInfo where
    toSrcFileSpan = toSrcFileSpan . Exts.srcInfoSpan

-- | 
instance Spannable Exts.Comment where
    toSrcSpan (Exts.Comment _ s _) = toSrcSpan s
