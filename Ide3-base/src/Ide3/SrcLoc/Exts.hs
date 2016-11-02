{-|
Module      : Ide3.SrcLoc.Exts
Description : Converting source locations from haskell-src-exts types
Copyright   : (c) Andrew Melnick, 2016

License     : BSD3
Maintainer  : meln5674@kettering.edu
Stability   : experimental
Portability : POSIX
-}
module Ide3.SrcLoc.Exts where

import Prelude hiding (span)

import qualified Language.Haskell.Exts.SrcLoc as Exts
import qualified Language.Haskell.Exts.Comments as Exts

import Ide3.SrcLoc.Types


-- | Extract row and column
instance Loccable Exts.SrcLoc where
    toSrcLoc (Exts.SrcLoc _ a b) = SrcLoc (Row a) (Column b)

-- | Extract filename, then extract from location
instance FileLoccable Exts.SrcLoc where
    toSrcFileLoc loc = SrcFileLoc (Exts.srcFilename loc) $ toSrcLoc loc

-- | Create a span from the start and end row/columns
instance Spannable Exts.SrcSpan where
    toSrcSpan (Exts.SrcSpan _ a b c d)
        = mkSrcSpan (Row a) (Column b) (Row c) (Column d)

-- | Extract filename, then extract from span
instance FileSpannable Exts.SrcSpan where
    toSrcFileSpan span =
        SrcFileSpan (Exts.srcSpanFilename span) $ toSrcSpan span

-- | Extract from overall span
instance Spannable Exts.SrcSpanInfo where
    toSrcSpan = toSrcSpan . Exts.srcInfoSpan

-- | Extract filename, then extract from span
instance FileSpannable Exts.SrcSpanInfo where
    toSrcFileSpan = toSrcFileSpan . Exts.srcInfoSpan

-- | Get span of comment
instance Spannable Exts.Comment where
    toSrcSpan (Exts.Comment _ s _) = toSrcSpan s
