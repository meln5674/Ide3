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
    , module Ide3.SrcLoc.Types
    ) where

import Prelude hiding (span)

import Data.Text (Text)
import qualified Data.Text as T

import Ide3.SrcLoc.Types

-- | Extract a substring
(><) :: Spannable l => l -> Text -> Text
x >< str = T.take len $ T.drop startIndex str
  where
    span = toSrcSpan x
    start = spanStart span
    end = spanEnd span
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

-- | Given a 1-based (row,column) pair, find the 0-based character index in a
-- string
indexIn :: SrcLoc -> Text -> Maybe Int
(SrcLoc (Row 1) (Column c)) `indexIn` _ = Just $ c - 1
(SrcLoc (Row r) (Column c)) `indexIn` str = do
    let (line,_) = T.break (\c -> c == '\n') str
        lineLen = 1 + T.length line
    next <- (SrcLoc (Row (r-1)) (Column c)) `indexIn` T.drop lineLen str
    return $ lineLen + next

-- | Take a string, break it into lines, and take the length of each one
measureLines :: Text -> [(Text,Int)]
measureLines = map (\l -> (l,T.length l + 1)) . T.lines

-- | Test if two source spans meet, i.e one ends where the other begins
contacts :: SrcSpan -> SrcSpan -> Text -> Bool
contacts a b s = case (a2i,b1i) of
    (Just i1,Just i2) -> 0 <= i2-i1 && i2-i1 <= 1
    _ -> False
  where
    a2 = spanEnd a
    b1 = spanStart b
    a2i = a2 `indexIn` s
    b1i = b1 `indexIn` s

-- | Test if two source spans start at the same place
sameStart :: SrcSpan -> SrcSpan -> Text -> Bool
sameStart a b s = case (a2i,b1i) of
    (Just i1,Just i2) -> 0 <= i2-i1 && i2-i1 <= 1
    _ -> False
  where
    a2 = spanStart a
    b1 = spanStart b
    a2i = a2 `indexIn` s
    b1i = b1 `indexIn` s

-- | Test if two source spans end at the same place
sameEnd :: SrcSpan -> SrcSpan -> Text -> Bool
sameEnd a b s = case (a2i,b1i) of
    (Just i1,Just i2) -> 0 <= i2-i1 && i2-i1 <= 1
    _ -> False
  where
    a2 = spanEnd a
    b1 = spanEnd b
    a2i = a2 `indexIn` s
    b1i = b1 `indexIn` s

-- | Test if a src 
contains :: (Spannable s, Loccable l) => s -> l -> Bool
a `contains` b = aStart <= b' && b' <= aEnd
  where
    (SrcSpan aStart aEnd) = toSrcSpan a
    b' = toSrcLoc b
    
-- | Filter out from a list of spannables those which contact another spannable
-- on the left side
leftBoundaries :: (Spannable a, Spannable b) => Text -> b -> [a] -> [a]
leftBoundaries s y = filter $ \x -> contacts (toSrcSpan x) (toSrcSpan y) s

-- | Filter out from a list of spannables those which contact another spannable
-- on the right side
rightBoundaries :: (Spannable a, Spannable b) => Text -> b -> [a] -> [a]
rightBoundaries s y = filter $ \x -> contacts (toSrcSpan y) (toSrcSpan x) s

-- | Filter out from a list of spannables those which contact another spannable
boundaries :: (Spannable a, Spannable b) => Text -> b -> [a] -> [a]
boundaries s y = filter $ \x -> contacts (toSrcSpan x) (toSrcSpan y) s
                             || contacts (toSrcSpan y) (toSrcSpan x) s

-- | Filter out from a list of spannables those which intersect another
-- spannable
intersectors :: (Spannable a, Spannable b) => Text -> b -> [a] -> [a]
intersectors s y = filter $ \x -> sameEnd (toSrcSpan x) (toSrcSpan y) s
                               || sameStart (toSrcSpan x) (toSrcSpan y) s

-- | Subtract the second span from the first
subtractSrcSpan :: (Spannable a, Spannable b) => a -> b -> Text -> SrcSpan
subtractSrcSpan a' b' s
  | b1i <= a1i && a1i <= b2i && b2i <= a2i = mkSrcSpan b2r b2c a2r a2c
  | a1i <= b1i && b1i <= a2i && a2i <= b2i = mkSrcSpan a1r a1c b1r b1c
  | otherwise = a
  where
    a@(SrcSpan a1 a2) = toSrcSpan a'
    (SrcSpan b1 b2) = toSrcSpan b'
    a1i = a1 `indexIn` s
    a2i = a2 `indexIn` s
    b1i = b1 `indexIn` s
    b2i = b2 `indexIn` s
    (SrcLoc a1r a1c) = a1
    (SrcLoc a2r a2c) = a2
    (SrcLoc b1r b1c) = b1
    (SrcLoc b2r b2c) = b2
