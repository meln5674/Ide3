module Ide3.Constructor where

import Ide3.Types

import Language.Haskell.Exts.Syntax hiding (Symbol)

class ToConstructor a where
    toConstructor :: a -> Constructor

instance ToConstructor ConDecl where
    toConstructor (ConDecl n ts) = PrefixConstructor (toSym n) (map toSym ts)
    toConstructor (InfixConDecl tl n tr) = InfixConstructor (toSym tl) (toSym n) (toSym tr)
    toConstructor (RecDecl n rs) = RecordConstructor (toSym n) (concatMap f rs)
      where
        f (ns, t) = map (\n -> (toSym n,toSym t)) ns

instance ToConstructor QualConDecl where
    toConstructor (QualConDecl _ _ _ d) = toConstructor d

symbol :: Constructor -> Symbol
symbol (PrefixConstructor s _) = s
symbol (InfixConstructor _ s _) = s
symbol (RecordConstructor s _) = s

bindsProvided :: Constructor -> [Symbol]
bindsProvided (PrefixConstructor _ _) = []
bindsProvided (InfixConstructor _ _ _) = []
bindsProvided (RecordConstructor _ ss) = map fst ss

