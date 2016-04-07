module Ide3.Constructor where

import Ide3.Types

import Language.Haskell.Exts.Annotated.Syntax hiding (Symbol)
import Language.Haskell.Exts.SrcLoc

class ToConstructor a where
    toConstructor :: a -> Constructor

instance SrcInfo a => ToConstructor (ConDecl a) where
    toConstructor (ConDecl _ n ts) = PrefixConstructor (toSym n) (map toSym ts)
    toConstructor (InfixConDecl _ tl n tr) = InfixConstructor (toSym tl) (toSym n) (toSym tr)
    toConstructor (RecDecl _ n rs) = RecordConstructor (toSym n) (concatMap f rs)
      where
        f (FieldDecl _ ns t) = map (\n -> (toSym n,toSym t)) ns

instance SrcInfo a => ToConstructor (QualConDecl a) where
    toConstructor (QualConDecl _ _ _ d) = toConstructor d

symbol :: Constructor -> Symbol
symbol (PrefixConstructor s _) = s
symbol (InfixConstructor _ s _) = s
symbol (RecordConstructor s _) = s

bindsProvided :: Constructor -> [Symbol]
bindsProvided (PrefixConstructor _ _) = []
bindsProvided (InfixConstructor _ _ _) = []
bindsProvided (RecordConstructor _ ss) = map fst ss

