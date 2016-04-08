{-|
Module      : Ide3.Declaration.ModifierDeclaration
Description : TODO
Copyright   : (c) Andrew Melnick, 2016

License     : BSD3
Maintainer  : meln5674@kettering.edu
Stability   : experimental
Portability : POSIX

TODO
-}
module Ide3.Declaration.ModifierDeclaration where

import Ide3.Types

symbolsAffected :: ModifierDeclaration -> [Symbol]
symbolsAffected (FixityDeclaration ss _ _) = ss
symbolsAffected (InstanceDeclaration _ ss _) = ss
