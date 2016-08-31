module Ide3.Constructor.Exts where

import Language.Haskell.Exts.Annotated.Syntax hiding (Symbol)
import Language.Haskell.Exts.SrcLoc

import Ide3.Types.Internal
import Ide3.Types.Exts ()


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

-- |
instance SrcInfo a => ToConstructor (GadtDecl a) where
    toConstructor (GadtDecl _ n (Just rs) _) = RecordConstructor (toSym n) (concatMap f rs)
      where
        f (FieldDecl _ fns t) = map (\fn -> (toSym fn,toSym t)) fns
    toConstructor (GadtDecl _ n Nothing t) = PrefixConstructor (toSym n) [toSym t]


