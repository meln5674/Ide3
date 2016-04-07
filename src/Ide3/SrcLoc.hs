module Ide3.SrcLoc ( module Ide3.SrcLoc, module Language.Haskell.Exts.SrcLoc ) where

import Language.Haskell.Exts.SrcLoc

class Spanable a where
    (><) :: a -> String -> String


splitAndCount :: Eq a => [a] -> a -> (Int,[a])
splitAndCount = go 0
  where
    go _ [] _ = (-1,[])
    go i (x:xs) y | x == y      = (i+1, xs)
                  | otherwise   = go (i+1) xs y

indexIn :: (Int,Int) -> String -> Int
(1,c) `indexIn` str = c - 1
(r,c) `indexIn` str = lineLen + ((r-1,c) `indexIn` drop lineLen str)
  where
   (lineLen,rest) = splitAndCount str '\n'

instance Spanable SrcSpan where
    span >< str = take len $ drop startIndex str
      where
        start = srcSpanStart span
        end = srcSpanEnd span
        startIndex = start `indexIn` str
        endIndex = end `indexIn` str
        len = endIndex - startIndex

instance Spanable SrcSpanInfo where
    (SrcSpanInfo{srcInfoSpan=span}) >< str = span >< str

