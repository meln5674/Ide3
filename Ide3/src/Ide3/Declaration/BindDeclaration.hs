{-|
Module      : Ide3.Declaration.BindDeclaration
Description : Operations on bind declarations
Copyright   : (c) Andrew Melnick, 2016

License     : BSD3
Maintainer  : meln5674@kettering.edu
Stability   : experimental
Portability : POSIX

A bind declaration is a function or pattern bind, or a foreign function import
-}
module Ide3.Declaration.BindDeclaration where

import Ide3.Types

symbolsCreated :: BindDeclaration -> [Symbol]
symbolsCreated (LocalBindDeclaration ss _) = ss
symbolsCreated (ForeignBindDeclaration s _ _) = [s]

