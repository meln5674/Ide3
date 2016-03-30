module Ide3.Declaration.BindDeclaration where

import Ide3.Types

symbolCreated :: BindDeclaration -> Symbol
symbolCreated (LocalBindDeclaration s _) = s
symbolCreated (ForeignBindDeclaration s _ _) = s

