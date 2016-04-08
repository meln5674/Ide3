{-|
Module      : Ide3.Declaration.BindDeclaration
Description : TODO
Copyright   : (c) Andrew Melnick, 2016

License     : BSD3
Maintainer  : meln5674@kettering.edu
Stability   : experimental
Portability : POSIX

TODO
-}
module Ide3.Declaration.BindDeclaration where

import Ide3.Types

symbolsCreated :: BindDeclaration -> [Symbol]
symbolsCreated (LocalBindDeclaration ss _) = ss
symbolsCreated (ForeignBindDeclaration s _ _) = [s]

