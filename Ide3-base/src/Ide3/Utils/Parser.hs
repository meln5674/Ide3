{-|
Module      : Ide3.Utils.Parser
Description : Parser Utilities
Copyright   : (c) Andrew Melnick, 2016

License     : BSD3
Maintainer  : meln5674@kettering.edu
Stability   : experimental
Portability : POSIX
-}

module Ide3.Utils.Parser where

-- | Annotated data
data Ann l x = Ann l x

-- | Remove annotation
unAnn :: Ann l x -> x
unAnn (Ann _ x) = x

-- | Retrieve annotation
ann :: Ann l x -> l
ann (Ann l _) = l

-- | Apply a transformation to an annotation
mapAnn :: (l -> l') -> Ann l x -> Ann l' x
mapAnn f (Ann l x) = Ann (f l) x

-- | Applies f to the annotated data
instance Functor (Ann l) where
    fmap f (Ann l x) = Ann l (f x)
