module Ide3.Utils.Parser where

data Ann l x = Ann l x

unAnn :: Ann l x -> x
unAnn (Ann l x) = x

ann :: Ann l x -> l
ann (Ann l _) = l

mapAnn :: (l -> l') -> Ann l x -> Ann l' x
mapAnn f (Ann l x) = Ann (f l) x

instance Functor (Ann l) where
    fmap f (Ann l x) = Ann l (f x)
