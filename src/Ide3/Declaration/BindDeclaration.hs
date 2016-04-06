module Ide3.Declaration.BindDeclaration where

import Ide3.Types

symbolsCreated :: BindDeclaration -> [Symbol]
symbolsCreated (LocalBindDeclaration ss _) = ss
symbolsCreated (ForeignBindDeclaration s _ _) = [s]

