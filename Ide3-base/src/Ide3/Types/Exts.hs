{-|
Module      : Ide3.Types.Exts
Description : Converting from haskell-src-ext types
Copyright   : (c) Andrew Melnick, 2016

License     : BSD3
Maintainer  : meln5674@kettering.edu
Stability   : experimental
Portability : POSIX
-}

{-# LANGUAGE OverloadedStrings #-}
module Ide3.Types.Exts where

import Data.Monoid

import qualified Data.Text as T

import Language.Haskell.Exts.Syntax hiding (Symbol, Module, Type)
import qualified Language.Haskell.Exts.Syntax as Syntax
import Language.Haskell.Exts.SrcLoc

import Language.Haskell.Exts.Pretty

import Ide3.Types.Internal

-- | Convert identifiers and symbols
instance ToSym (Name a) where
    toSym (Ident _ n)         = Symbol $ T.pack n
    toSym (Syntax.Symbol _ n) = Symbol $ T.pack n

-- | Extract name
instance ToSym (CName a) where
    toSym (VarName _ n) = toSym n
    toSym (ConName _ n) = toSym n

-- | Extract module name
instance ToSym (ModuleName a) where
    toSym (ModuleName _ n) = Symbol $ T.pack n

-- | Extract syntatic constructors
instance ToSym (SpecialCon a) where
    toSym (UnitCon _)   = Symbol "()"
    toSym (ListCon _)   = Symbol "[]"
    toSym (FunCon _)    = Symbol "->"
    toSym (TupleCon _ Unboxed n) = Symbol $ "(" <> T.replicate n "," <> ")"
    toSym (TupleCon _ Boxed n) = Symbol $ "(#" <> T.replicate n "," <> "#)"
    toSym (Cons _) = Symbol ":"
    toSym (UnboxedSingleCon _) = Symbol "(# #)"

-- | Extract qualified names and join them, extract unqualified names normally
instance ToSym (QName a) where
    toSym (Qual _ m n) = toSym m `joinSym` toSym n
    toSym (UnQual _ n) = toSym n
    toSym (Special _ s) = toSym s

-- | Convert to symbol using the pretty printer
instance ToSym (Syntax.Type a) where
    toSym = Symbol . T.pack . prettyPrint

-- | Extract recursively until the head is reached
instance ToSym (DeclHead a) where
    toSym (DHead _ n) = toSym n
    toSym (DHInfix _ _ n) = toSym n
    toSym (DHParen _ h) = toSym h
    toSym (DHApp _ h _) = toSym h

instance ToSym (Op a) where
    toSym (VarOp _ n) = toSym n
    toSym (ConOp _ n) = toSym n

-- | Search heads for names
instance SrcInfo a => HasNames (InstRule a) where
    findName (IRule _ _ _ h) = findName h
    findName (IParen _ h) = findName h

-- | Take names from head
instance SrcInfo a => HasNames (InstHead a) where
    findName (IHCon _ n) = [toSym n]
    findName (IHInfix _ t n) = [toSym t, toSym n]
    findName (IHParen _ h) = findName h
    findName (IHApp _ h t) = toSym t : findName h

-- | Recursively find names in nested patterns
instance HasNames (Pat a) where
    findName (PVar _ n) = [toSym n]
    findName (PNPlusK _ n _) = [toSym n]
    findName (PInfixApp _ p1 _ p2) = findName p1 ++ findName p2
    findName (PApp _ _ ps) = concatMap findName ps
    findName (PTuple _ _ ps) = concatMap findName ps
    findName (PList _ ps) = concatMap findName ps
    findName (PRec _ _ fs) = concatMap findName fs
    findName (PAsPat _ n _) = [toSym n]
    findName (PIrrPat _ p) = findName p
    findName (PatTypeSig _ p _) = findName p
    findName _ = [] -- TODO: rest

-- | Find names in pattern fields
instance HasNames (PatField l) where
    findName (PFieldPat _ _ p) = findName p
    findName (PFieldPun _ n) = [toSym n]
    findName _ = [] -- TODO: rest

