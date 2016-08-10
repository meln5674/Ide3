{-|
Module      : Ide3.SrcLoc
Description : Utilities for source locations
Copyright   : (c) Andrew Melnick, 2016

License     : BSD3
Maintainer  : meln5674@kettering.edu
Stability   : experimental
Portability : POSIX

-}

module Ide3.SrcLoc
    ( module Ide3.SrcLoc
    , module Language.Haskell.Exts.SrcLoc 
    ) where

import Prelude hiding (span)

import Language.Haskell.Exts.SrcLoc

-- | The class of types which can be used to retreive a substring
class Spannable a where
    -- | Convert to the SrcSpan type
    getSpan :: a -> SrcSpan
    -- | Extract a substring
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

-- | Test if two source spans start at the same place
sameStart :: SrcSpan -> SrcSpan -> String -> Bool
sameStart a b s = case (a2i,b1i) of
    (Just i1,Just i2) -> 0 <= i2-i1 && i2-i1 <= 1
    _ -> False
  where
    a2 = srcSpanStart a
    b1 = srcSpanStart b
    a2i = a2 `indexIn` s
    b1i = b1 `indexIn` s

-- | Test if two source spans end at the same place
sameEnd :: SrcSpan -> SrcSpan -> String -> Bool
sameEnd a b s = case (a2i,b1i) of
    (Just i1,Just i2) -> 0 <= i2-i1 && i2-i1 <= 1
    _ -> False
  where
    a2 = srcSpanEnd a
    b1 = srcSpanEnd b
    a2i = a2 `indexIn` s
    b1i = b1 `indexIn` s

-- | Filter out from a list of spannables those which contact another spannable
-- on the left side
leftBoundaries :: (Spannable a, Spannable b) => String -> b -> [a] -> [a]
leftBoundaries s y = filter $ \x -> contacts (getSpan x) (getSpan y) s

-- | Filter out from a list of spannables those which contact another spannable
-- on the right side
rightBoundaries :: (Spannable a, Spannable b) => String -> b -> [a] -> [a]
rightBoundaries s y = filter $ \x -> contacts (getSpan y) (getSpan x) s

-- | Filter out from a list of spannables those which contact another spannable
boundaries :: (Spannable a, Spannable b) => String -> b -> [a] -> [a]
boundaries s y = filter $ \x -> contacts (getSpan x) (getSpan y) s
                             || contacts (getSpan y) (getSpan x) s

-- | Filter out from a list of spannables those which intersect another spannable
intersectors :: (Spannable a, Spannable b) => String -> b -> [a] -> [a]
intersectors s y = filter $ \x -> sameEnd (getSpan x) (getSpan y) s
                               || sameStart (getSpan x) (getSpan y) s

-- | Subtract the second span from the first
subtractSrcSpan :: (Spannable a, Spannable b) => a -> b -> String -> SrcSpan
subtractSrcSpan a' b' s
  | b1i <= a1i && a1i <= b2i && b2i <= a2i = SrcSpan filename b2l b2c a2l a2c
  | a1i <= b1i && b1i <= a2i && a2i <= b2i = SrcSpan filename a1l a1c b1l b1c
  | otherwise = a
  where
    a = getSpan a'
    b = getSpan b'
    a1 = srcSpanStart a
    a2 = srcSpanEnd a
    b1 = srcSpanStart b
    b2 = srcSpanEnd b
    a1i = a1 `indexIn` s
    a2i = a2 `indexIn` s
    b1i = b1 `indexIn` s
    b2i = b2 `indexIn` s
    filename = srcSpanFilename a
    a1l = fst a1
    a1c = snd a1
    a2l = fst a2
    a2c = snd a2
    b1l = fst b1
    b1c = snd b1
    b2l = fst b2
    b2c = snd b2

-- | 
instance Spannable SrcSpan where
    getSpan = id

-- | 
instance Spannable SrcSpanInfo where
    getSpan = srcInfoSpan
