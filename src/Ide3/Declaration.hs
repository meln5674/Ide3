module Ide3.Declaration where

import Control.Applicative

import Data.List
import Data.Monoid

import Language.Haskell.Exts.Parser
import Language.Haskell.Exts.Syntax hiding (Symbol)
import qualified Language.Haskell.Exts.Syntax as Syntax

import Ide3.Types
import qualified Ide3.Constructor as Constructor

import qualified Ide3.Declaration.TypeDeclaration as TypeDeclaration
import qualified Ide3.Declaration.BindDeclaration as BindDeclaration
import qualified Ide3.Declaration.ModifierDeclaration as ModifierDeclaration

import qualified Data.Map as Map
import Data.Map (Map)

info :: Declaration -> DeclarationInfo
info (TypeDeclaration i _) = i
info (BindDeclaration i _) = i
info (ModifierDeclaration i _) = i

parseTypeSynonym (TypeDecl _ n _ t)
    = Just $ TypeDeclaration (DeclarationInfo (toSym n))
                             (TypeSynonym (toSym n)
                                          (toSym t)
                             )
parseTypeSynonym _ = Nothing

parseDataDecl (DataDecl _ NewType context n _ [con] dervs)
    = Just $ TypeDeclaration (DeclarationInfo (toSym n))
                             (NewtypeDeclaration (toSym n)
                                                 (Constructor.toConstructor con)
                             )
parseDataDecl _ = Nothing

parseNewtypeDecl (DataDecl _ DataType context n _ cons dervs)
    = Just $ TypeDeclaration (DeclarationInfo (toSym n))
                             (DataDeclaration (toSym n)
                                              (map Constructor.toConstructor cons)
                             )
parseNewtypeDecl _ = Nothing

parseFuncBind (FunBind (m:_)) = Just $ BindDeclaration (DeclarationInfo $ toSym n) 
                                     $ LocalBindDeclaration [toSym n] Nothing
  where
    Match _ n _ _ _ _ = m
parseFuncBind _ = Nothing

parseTypeSignature (TypeSig _ ns t) = case allsigs of
    Nothing -> Nothing
    Just [] -> Nothing
    Just [x] -> Just x
    Just (x:xs) -> Just x --TODO
  where
    for xs f = map f xs
    allsigs = Just $ for ns $ \n ->
        ModifierDeclaration (DeclarationInfo $ toSym n)
      $ TypeSignatureDeclaration (toSym n) (toSym t)
parseTypeSignature _ = Nothing

parseClassDecl (ClassDecl _ _ n _ _ ds)
    = Just $ TypeDeclaration (DeclarationInfo $ toSym n)
           $ ClassDeclaration (toSym n) ds'
  where
    parseSubDecl (ClsDecl d) = tryConvert d
    parseSubDecl _ = Nothing
    Just ds' = sequence $ map parseSubDecl ds
parseClassDecl _ = Nothing

--parsePatBind (PatBind _ p _ _) = Just $ BindDeclaration


tryConvert x = getFirst $ mconcat                 
             $ map (First . ($x))
                [ parseTypeSynonym
                , parseDataDecl
                , parseNewtypeDecl
                , parseFuncBind
                , parseClassDecl
                , parseTypeSignature
                ]
          

parseMany :: String -> Either String [Declaration]
parseMany s = case parseModule s of
    ParseOk (Syntax.Module _ _ _ _ _ _ ds) -> case sequence $ map tryConvert ds of
        Just y -> Right y
        Nothing -> Left "Unsupported"
    ParseFailed _ s -> Left s


partitionBy :: (Ord k) => (a -> k) -> [a] -> Map k [a]
partitionBy f xs = go xs Map.empty
  where
    go [] m = m
    go (x:xs) m = go xs m'
      where
        k = f x
        v' = case Map.lookup k m of
            Just v -> x:v
            Nothing -> [x]
        m' = Map.insert k v' m

combineMany :: [Declaration] -> [Declaration]
combineMany ds = ds'
  where
    m = partitionBy info ds
    m' = Map.map combineFuncAndTypeSig m
    ds' = concat $ Map.elems m'

combineFuncAndTypeSig :: [Declaration] -> [Declaration]
combineFuncAndTypeSig ds = case (typeSigs,funcBinds) of
    ((sig:_),(func:_)) -> BindDeclaration info (LocalBindDeclaration syms (Just type_)) : notFuncBinds
      where
        ModifierDeclaration _ (TypeSignatureDeclaration _ type_) = sig
        BindDeclaration info (LocalBindDeclaration syms _) = func
    _ -> ds
  where
    (typeSigs,notTypeSigs) = partition isTypeSig ds
    isTypeSig (ModifierDeclaration _ (TypeSignatureDeclaration _ _)) = True
    isTypeSig _ = False
    (funcBinds,notFuncBinds) = partition isFuncBind notTypeSigs
    isFuncBind (BindDeclaration _ (LocalBindDeclaration _ _)) = True
    isFuncBind _ = False
    
parse :: String -> Either String Declaration
parse s = case parseDecl s of
    ParseOk x -> case tryConvert x of
        Just y -> Right y
        Nothing -> Left $ "Unsupported: " ++ show x where
    ParseFailed _ s -> Left s

symbolsProvided :: Declaration -> [Symbol]
symbolsProvided t = typesProvided t 
                 ++ constructorsProvided t
                 ++ bindsProvided t

providesSymbol :: Declaration -> Symbol -> Bool
providesSymbol d s = s `elem` symbolsProvided d

otherSymbols :: Declaration -> Symbol -> Maybe [Symbol]
otherSymbols d s | d `providesSymbol` s = Just $ delete s $ symbolsProvided d
                 | otherwise = Nothing

typesProvided :: Declaration -> [Symbol]
typesProvided (TypeDeclaration _ t) = [TypeDeclaration.typeCreated t]
typesProvided _ = []

constructorsProvided :: Declaration -> [Symbol]
constructorsProvided (TypeDeclaration _ t)
    = TypeDeclaration.constructorsCreated t
constructorsProvided _ = []

bindsProvided :: Declaration -> [Symbol]
bindsProvided (BindDeclaration _ t) = BindDeclaration.symbolsCreated t
bindsProvided (TypeDeclaration _ t) = TypeDeclaration.bindsCreated t
bindsProvided _ = []

symbolsAffected :: Declaration -> [Symbol]
symbolsAffected (ModifierDeclaration _ t) = ModifierDeclaration.symbolsAffected t
symbolsAffected _ = []

affectsSymbol :: Declaration -> Symbol -> Bool
d `affectsSymbol` s = s `elem` symbolsAffected d

