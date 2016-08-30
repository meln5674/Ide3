module Ide3.Utils.Parser where

data Ann l x = Ann l x

unAnn :: Ann l x -> x
unAnn (Ann l x) = x

ann :: Ann l x -> l
ann (Ann l _) = l
