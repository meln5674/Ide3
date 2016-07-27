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
class Spannable a where
    getSpan :: a -> SrcSpan
    (><) :: a -> String -> String
    x >< str = take len $ drop startIndex str
      where
        span = getSpan x
        start = srcSpanStart span
        end = srcSpanEnd span
        Just startIndex = start `indexIn` str
        Just endIndex = end `indexIn` str
        len = endIndex - startIndex
    
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
    (lineLen,_) <- splitAndCount str '\n'
    next <- (r-1,c) `indexIn` drop lineLen str
    return $ lineLen + next

-- | Take a string, break it into lines, and take the length of each one
measureLines :: String -> [(String,Int)]
measureLines = map (\l -> (l,length l + 1)) . lines

-- | Test if two source spans meet, i.e one ends where the other begins
contacts :: SrcSpan -> SrcSpan -> String -> Bool
contacts a b s = case (a2i,b1i) of
    (Just i1,Just i2) -> 0 <= i2-i1 && i2-i1 <= 1
    _ -> False
  where
    a2 = srcSpanEnd a
    b1 = srcSpanStart b
    a2i = a2 `indexIn` s
    b1i = b1 `indexIn` s

-- | Filter out from a list of spannables those which contact another spannable on the left side
leftBoundaries :: (Spannable a, Spannable b) => String -> b -> [a] -> [a]
leftBoundaries s y = filter $ \x -> contacts (getSpan x) (getSpan y) s

-- | Filter out from a list of spannables those which contact another spannable on the right side
rightBoundaries :: (Spannable a, Spannable b) => String -> b -> [a] -> [a]
rightBoundaries s y = filter $ \x -> contacts (getSpan y) (getSpan x) s

-- | Filter out from a list of spannables those which contact another spannable
boundaries :: (Spannable a, Spannable b) => String -> b -> [a] -> [a]
boundaries s y = filter $ \x -> contacts (getSpan x) (getSpan y) s
                             || contacts (getSpan y) (getSpan x) s
-- | 
instance Spannable SrcSpan where
    getSpan = id

-- | 
instance Spannable SrcSpanInfo where
    getSpan (SrcSpanInfo{srcInfoSpan=span}) = span

