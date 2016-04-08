{-|
Module      : Ide3.Declaration.TypeDeclaration
Description : TODO
Copyright   : (c) Andrew Melnick, 2016

License     : BSD3
Maintainer  : meln5674@kettering.edu
Stability   : experimental
Portability : POSIX

TODO
-}
module Ide3.Declaration.TypeDeclaration where

import Ide3.Types
import qualified Ide3.Constructor as Constructor

typeCreated :: TypeDeclaration -> Symbol
typeCreated (ClassDeclaration s _) = s
typeCreated (TypeSynonym s _) = s
typeCreated (DataDeclaration s _) = s
typeCreated (NewtypeDeclaration s _) = s

bindsCreated :: TypeDeclaration -> [Symbol]
bindsCreated (ClassDeclaration _ ds)
    = undefined -- concatMap symbolsProvided ds
bindsCreated (TypeSynonym _ _) = []
bindsCreated (DataDeclaration _ cs)
    = concatMap Constructor.bindsProvided cs
bindsCreated (NewtypeDeclaration _ c)
    = Constructor.bindsProvided c
    
constructorsCreated :: TypeDeclaration -> [Symbol]
constructorsCreated (ClassDeclaration _ _) = []
constructorsCreated (TypeSynonym _ _) = []
constructorsCreated (DataDeclaration _ cs)
    = map Constructor.symbol cs
constructorsCreated (NewtypeDeclaration _ c)
    = [Constructor.symbol c]
