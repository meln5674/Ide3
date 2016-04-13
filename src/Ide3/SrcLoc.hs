{-|
Module      : Ide3.SrcLoc
Description : Utilities for source locations
Copyright   : (c) Andrew Melnick, 2016

License     : BSD3
Maintainer  : meln5674@kettering.edu
Stability   : experimental
Portability : POSIX

-}
module Ide3.SrcLoc ( module Ide3.SrcLoc, module Language.Haskell.Exts.SrcLoc ) where

import Prelude hiding (span)

import Language.Haskell.Exts.SrcLoc

-- | The class of types which can be used to retreive a substring
class Spanable a where
    (><) :: a -> String -> String


-- | Scan a list for a matching item, then return the number of items scanned
-- and the remaining items
splitAndCount :: Eq a => [a] -> a -> Maybe (Int,[a])
splitAndCount = go 0
  where
    go _ [] _ = Nothing
    go i (x:xs) y | x == y      = Just (i+1, xs)
                  | otherwise   = go (i+1) xs y

-- | Given a 1-based (row,column) pair, find the 0-based character index in a string
indexIn :: (Int,Int) -> String -> Maybe Int
(1,c) `indexIn` _ = Just $ c - 1
(r,c) `indexIn` str = do
    (lineLen,rest) <- splitAndCount str '\n'
    next <- ((r-1,c) `indexIn` drop lineLen str)
    return $ lineLen + next

instance Spanable SrcSpan where
    span >< str = take len $ drop startIndex str
      where
        start = srcSpanStart span
        end = srcSpanEnd span
        Just startIndex = start `indexIn` str
        Just endIndex = end `indexIn` str
        len = endIndex - startIndex

instance Spanable SrcSpanInfo where
    (SrcSpanInfo{srcInfoSpan=span}) >< str = span >< str

