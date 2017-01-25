{-|
Module      : Ide3.Declaration.TypeDeclaration
Description : Operations on type declarations
Copyright   : (c) Andrew Melnick, 2016

License     : BSD3
Maintainer  : meln5674@kettering.edu
Stability   : experimental
Portability : POSIX

A type declaration is any declaration that provides new types to the program:

data
type
newtype
class
-}
module Ide3.Declaration.TypeDeclaration where

import Ide3.Types.Internal
import qualified Ide3.Constructor as Constructor
import {-# SOURCE #-} qualified Ide3.Declaration as Declaration

-- | Get the type created by a declaration
typeCreated :: TypeDeclaration -> Symbol
typeCreated (ClassDeclaration s _) = s
typeCreated (TypeSynonym s _) = s
typeCreated (DataDeclaration s _) = s
typeCreated (NewtypeDeclaration s _) = s
typeCreated (OpenTypeFamilyDecl s) = s
typeCreated (ClosedTypeFamilyDecl s _) = s

-- | Get the list of binds created by a declaration
bindsCreated :: TypeDeclaration -> [Symbol]
bindsCreated (ClassDeclaration _ ds)
    = concatMap Declaration.symbolsProvided ds
bindsCreated (TypeSynonym _ _) = []
bindsCreated (DataDeclaration _ cs)
    = concatMap Constructor.bindsProvided cs
bindsCreated (NewtypeDeclaration _ c)
    = Constructor.bindsProvided c
bindsCreated (OpenTypeFamilyDecl _) = []
bindsCreated (ClosedTypeFamilyDecl _ _) = []

-- | Get a list of constructors created by a declaration
constructorsCreated :: TypeDeclaration -> [Symbol]
constructorsCreated (ClassDeclaration _ _) = []
constructorsCreated (TypeSynonym _ _) = []
constructorsCreated (DataDeclaration _ cs)
    = map Constructor.symbol cs
constructorsCreated (NewtypeDeclaration _ c)
    = [Constructor.symbol c]
constructorsCreated (OpenTypeFamilyDecl _) = []
constructorsCreated (ClosedTypeFamilyDecl _ _) = []
