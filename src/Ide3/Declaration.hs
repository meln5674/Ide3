{-|
Module      : Ide3.Declaration
Description : TODO
Copyright   : (c) Andrew Melnick, 2016

License     : BSD3
Maintainer  : meln5674@kettering.edu
Stability   : experimental
Portability : POSIX

TODO
-}
module Ide3.Declaration
    ( module Ide3.Declaration
    , Parser.parse
    , Parser.convertWithBody
    ) where

import Control.Applicative

import Data.List

import Ide3.Types
import qualified Ide3.Declaration.Parser as Parser

import qualified Ide3.Declaration.TypeDeclaration as TypeDeclaration
import qualified Ide3.Declaration.BindDeclaration as BindDeclaration
import qualified Ide3.Declaration.ModifierDeclaration as ModifierDeclaration

import qualified Data.Map as Map
import Data.Map (Map)



partitionBy :: (Ord k) => (a -> k) -> [a] -> Map k [a]
partitionBy f xs = go xs Map.empty
  where
    go [] m = m
    go (x:xs) m = go xs m'
      where
        k = f x
        v' = case Map.lookup k m of
            Just v -> x:v
            Nothing -> [x]
        m' = Map.insert k v' m

combineMany :: [WithBody Declaration] -> [WithBody Declaration]
combineMany ds = ds'
  where
    m = partitionBy (info . item) ds
    m' = Map.map Parser.combineFuncAndTypeSig m
    ds' = concat $ Map.elems m'


info :: Declaration -> DeclarationInfo
info (TypeDeclaration i _) = i
info (BindDeclaration i _) = i
info (ModifierDeclaration i _) = i


symbolsProvided :: Declaration -> [Symbol]
symbolsProvided t = typesProvided t 
                 ++ constructorsProvided t
                 ++ bindsProvided t

providesSymbol :: Declaration -> Symbol -> Bool
providesSymbol d s = s `elem` symbolsProvided d

otherSymbols :: Declaration -> Symbol -> Maybe [Symbol]
otherSymbols d s | d `providesSymbol` s = Just $ delete s $ symbolsProvided d
                 | otherwise = Nothing

typesProvided :: Declaration -> [Symbol]
typesProvided (TypeDeclaration _ t) = [TypeDeclaration.typeCreated t]
typesProvided _ = []

constructorsProvided :: Declaration -> [Symbol]
constructorsProvided (TypeDeclaration _ t)
    = TypeDeclaration.constructorsCreated t
constructorsProvided _ = []

bindsProvided :: Declaration -> [Symbol]
bindsProvided (BindDeclaration _ t) = BindDeclaration.symbolsCreated t
bindsProvided (TypeDeclaration _ t) = TypeDeclaration.bindsCreated t
bindsProvided _ = []

symbolsAffected :: Declaration -> [Symbol]
symbolsAffected (ModifierDeclaration _ t) = ModifierDeclaration.symbolsAffected t
symbolsAffected _ = []

affectsSymbol :: Declaration -> Symbol -> Bool
d `affectsSymbol` s = s `elem` symbolsAffected d

