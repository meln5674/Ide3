{-|
Module      : Ide3.Constructor
Description : Operations on constructors
Copyright   : (c) Andrew Melnick, 2016

License     : BSD3
Maintainer  : meln5674@kettering.edu
Stability   : experimental
Portability : POSIX
-}
module Ide3.Constructor where

import Ide3.Types

import Language.Haskell.Exts.Annotated.Syntax hiding (Symbol)
import Language.Haskell.Exts.SrcLoc

-- | Class of types which can be transformed into constructors
class ToConstructor a where
    toConstructor :: a -> Constructor

-- | 
instance SrcInfo a => ToConstructor (ConDecl a) where
    toConstructor (ConDecl _ n ts) = PrefixConstructor (toSym n) (map toSym ts)
    toConstructor (InfixConDecl _ tl n tr) = InfixConstructor (toSym tl) (toSym n) (toSym tr)
    toConstructor (RecDecl _ n rs) = RecordConstructor (toSym n) (concatMap f rs)
      where
        f (FieldDecl _ fns t) = map (\fn -> (toSym fn,toSym t)) fns

-- | 
instance SrcInfo a => ToConstructor (QualConDecl a) where
    toConstructor (QualConDecl _ _ _ d) = toConstructor d

instance SrcInfo a => ToConstructor (GadtDecl a) where
    toConstructor (GadtDecl _ n (Just rs) _) = RecordConstructor (toSym n) (concatMap f rs)
      where
        f (FieldDecl _ fns t) = map (\fn -> (toSym fn,toSym t)) fns
    toConstructor (GadtDecl _ n Nothing t) = PrefixConstructor (toSym n) [toSym t]

-- | Get the symbol of a constructor
symbol :: Constructor -> Symbol
symbol (PrefixConstructor s _) = s
symbol (InfixConstructor _ s _) = s
symbol (RecordConstructor s _) = s

-- | Get the binds provided by a constructor
bindsProvided :: Constructor -> [Symbol]
bindsProvided PrefixConstructor{} = []
bindsProvided InfixConstructor{} = []
bindsProvided (RecordConstructor _ ss) = map fst ss

