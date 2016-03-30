module Ide3.Declaration where

import Language.Haskell.Exts.Parser
import Language.Haskell.Exts.Syntax hiding (Symbol)

import Ide3.Types

import qualified Ide3.Declaration.TypeDeclaration as TypeDeclaration
import qualified Ide3.Declaration.BindDeclaration as BindDeclaration
import qualified Ide3.Declaration.ModifierDeclaration as ModifierDeclaration

info :: Declaration -> DeclarationInfo
info (TypeDeclaration i _) = i
info (BindDeclaration i _) = i
info (ModifierDeclaration i _) = i

parse :: String -> Either String Declaration
parse s = case parseDecl s of
    ParseOk x -> case x of
        TypeDecl _ n _ t -> Right $ TypeDeclaration DeclarationInfo 
                                            (TypeSynonym (Symbol $ show n)
                                                         (Symbol $ show t)
                                            )
    ParseFailed _ s -> Left s

symbolsProvided :: Declaration -> [Symbol]
symbolsProvided t = typesProvided t 
                 ++ constructorsProvided t
                 ++ bindsProvided t

typesProvided :: Declaration -> [Symbol]
typesProvided (TypeDeclaration _ t) = [TypeDeclaration.typeCreated t]
typesProvided _ = []

constructorsProvided :: Declaration -> [Symbol]
constructorsProvided (TypeDeclaration _ t)
    = TypeDeclaration.constructorsCreated t
constructorsProvided _ = []

bindsProvided :: Declaration -> [Symbol]
bindsProvided (BindDeclaration _ t) = [BindDeclaration.symbolCreated t]
bindsProvided (TypeDeclaration _ t) = TypeDeclaration.bindsCreated t
bindsProvided _ = []

symbolsAffected :: Declaration -> [Symbol]
symbolsAffected (ModifierDeclaration _ t) = ModifierDeclaration.symbolsAffected t
symbolsAffected _ = []

affectsSymbol :: Declaration -> Symbol -> Bool
d `affectsSymbol` s = s `elem` symbolsAffected d

