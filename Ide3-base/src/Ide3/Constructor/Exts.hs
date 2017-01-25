{-|
Module      : Ide3.Constructor.Exts
Description : Converting haskell-src-exts types to Constructors
Copyright   : (c) Andrew Melnick, 2016

License     : BSD3
Maintainer  : meln5674@kettering.edu
Stability   : experimental
Portability : POSIX
-}
module Ide3.Constructor.Exts where

import Language.Haskell.Exts.Syntax hiding (Symbol)

import Ide3.Types.Internal
import Ide3.Types.Exts ()


-- | Class of types which can be transformed into constructors
class ToConstructor a where
    toConstructor :: a -> Constructor

-- | Extract constructor from declaration
instance ToConstructor (ConDecl a) where
    toConstructor (ConDecl _ n ts) = PrefixConstructor (toSym n) (map toSym ts)
    toConstructor (InfixConDecl _ tl n tr) =
        InfixConstructor (toSym tl) (toSym n) (toSym tr)
    toConstructor (RecDecl _ n rs) =
        RecordConstructor (toSym n) (concatMap f rs)
      where
        f (FieldDecl _ fns t) = map (\fn -> (toSym fn,toSym t)) fns

-- | Extract constructor from declaration
instance ToConstructor (QualConDecl a) where
    toConstructor (QualConDecl _ _ _ d) = toConstructor d

-- | Extract constructor from GADT declaration
instance ToConstructor (GadtDecl a) where
    toConstructor (GadtDecl _ n (Just rs) _) =
        RecordConstructor (toSym n) (concatMap f rs)
      where
        f (FieldDecl _ fns t) = map (\fn -> (toSym fn,toSym t)) fns
    toConstructor (GadtDecl _ n Nothing t) =
        PrefixConstructor (toSym n) [toSym t]


