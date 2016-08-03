{-|
Module      : Ide3.Declaration.ModifierDeclaration
Description : Operations on modifier declarations
Copyright   : (c) Andrew Melnick, 2016

License     : BSD3
Maintainer  : meln5674@kettering.edu
Stability   : experimental
Portability : POSIX

A modifier declaration is any declaration which provides some property about
some other symbol in the program:

fixity declarations
class instances
-}
module Ide3.Declaration.ModifierDeclaration where

import Ide3.Types

-- | Get a list of symbols that a declaration affects
symbolsAffected :: ModifierDeclaration -> [Symbol]
symbolsAffected (FixityDeclaration ss _ _) = ss
symbolsAffected (InstanceDeclaration _ ss _) = ss
symbolsAffected (TypeSignatureDeclaration s _) = [s]
symbolsAffected (DerivingDeclaration _ ss) = ss
