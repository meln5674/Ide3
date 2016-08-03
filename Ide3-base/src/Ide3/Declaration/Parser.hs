{-|
Module      : Ide3.Declaration.Parser
Description : Parsing declarations
Copyright   : (c) Andrew Melnick, 2016

License     : BSD3
Maintainer  : meln5674@kettering.edu
Stability   : experimental
Portability : POSIX

This module provides functions for parsing declarations
-}
{-# LANGUAGE FlexibleInstances #-}
module Ide3.Declaration.Parser where

import Data.Monoid
import Data.List

import Language.Haskell.Exts.Annotated.Parser
import Language.Haskell.Exts.Annotated.Syntax hiding (Symbol)
import qualified Language.Haskell.Exts.Annotated.Syntax as Syntax
import Language.Haskell.Exts.Comments
import Language.Haskell.Exts.Pretty
import Language.Haskell.Exts.Parser
    ( ParseResult(..)
    , defaultParseMode
    , parseFilename
    , extensions
    , fixities
    )
import Language.Haskell.Exts.Extension


import Ide3.SrcLoc

import Ide3.Types
import qualified Ide3.Constructor as Constructor

-- | Convert a declaration if it is a type synonym
parseTypeSynonym :: SrcInfo t => Decl t -> Maybe Declaration
parseTypeSynonym (TypeDecl _ h t)
    = Just $ TypeDeclaration (DeclarationInfo (toSym h))
                             (TypeSynonym (toSym h)
                                          (toSym t)
                             )
parseTypeSynonym _ = Nothing

parseGADTNewtypeDecl :: SrcInfo t => Decl t -> Maybe Declaration
parseGADTNewtypeDecl (GDataDecl _ (NewType _) _ h _ [con] _)
    = Just $ TypeDeclaration (DeclarationInfo (toSym h))
                             (NewtypeDeclaration (toSym h)
                                              (Constructor.toConstructor con)
                             )
parseGADTNewtypeDecl _ = Nothing

-- | Convert a declaration if it is a newtype declaration
parseNewtypeDecl :: SrcInfo t => Decl t -> Maybe Declaration
parseNewtypeDecl (DataDecl _ (NewType _) _ h [con] _)
    = Just $ TypeDeclaration (DeclarationInfo (toSym h))
                             (NewtypeDeclaration (toSym h)
                                                 (Constructor.toConstructor con)
                             )
parseNewtypeDecl _ = Nothing

parseGADTDecl :: SrcInfo t => Decl t -> Maybe Declaration
parseGADTDecl (GDataDecl _ (DataType _) _ h _ cons _)
    = Just $ TypeDeclaration (DeclarationInfo (toSym h))
                             (DataDeclaration (toSym h)
                                              (map Constructor.toConstructor cons)
                             )
parseGADTDecl _ = Nothing

-- | Convert a declaration if it is a data declaration
parseDataDecl :: SrcInfo t => Decl t -> Maybe Declaration
parseDataDecl (DataDecl _ (DataType _) _ h cons _)
    = Just $ TypeDeclaration (DeclarationInfo (toSym h))
                             (DataDeclaration (toSym h)
                                              (map Constructor.toConstructor cons)
                             )
parseDataDecl _ = Nothing

-- | Convert a declaration if it is a function bind
parseFuncBind :: SrcInfo t => Decl t -> Maybe Declaration
parseFuncBind (FunBind _ (m:_)) = Just $ BindDeclaration (DeclarationInfo $ toSym n) 
                                     $ LocalBindDeclaration [toSym n] Nothing
  where
    n = case m of
        Match _ n' _ _ _ -> n'
        InfixMatch _ _ n' _ _ _ -> n'
parseFuncBind _ = Nothing

-- | Convert a declaration if it is a type signature
parseTypeSignature :: SrcInfo t => Decl t -> Maybe Declaration
parseTypeSignature (TypeSig _ ns t) = case allsigs of
    Nothing -> Nothing
    Just [] -> Nothing
    Just [x] -> Just x
    Just (x:_) -> Just x --TODO
  where
    for xs f = map f xs
    allsigs = Just $ for ns $ \n ->
        ModifierDeclaration (DeclarationInfo $ toSym n)
      $ TypeSignatureDeclaration (toSym n) (toSym t)
parseTypeSignature _ = Nothing

-- | Convert a declaration if it is a class declaration
parseClassDecl :: SrcInfo t => Decl t -> Maybe Declaration
parseClassDecl (ClassDecl _ _ h _ ds)
    = Just $ TypeDeclaration (DeclarationInfo $ toSym h)
           $ ClassDeclaration (toSym h) ds'
  where
    parseSubDecl (ClsDecl _ d) = tryConvert d
    parseSubDecl _ = Nothing
    Just ds' = ds >>= mapM parseSubDecl
parseClassDecl _ = Nothing

-- | Convert a declaration if it is an instance declaration
parseInstanceDecl :: SrcInfo t => Decl t -> Maybe Declaration
parseInstanceDecl (InstDecl _ _ r _)
    = Just $ ModifierDeclaration (DeclarationInfo $ Symbol $ prettyPrint r) 
           $ InstanceDeclaration cls ts []
  where
    (cls:ts) = findName r
parseInstanceDecl _ = Nothing

-- | Convert a declaration if it is a pattern bind
parsePatBind :: SrcInfo t => Decl t -> Maybe Declaration
parsePatBind (PatBind _ p _ _) = case findName p of
    [] -> Nothing
    ns@(n:_) -> Just $ BindDeclaration (DeclarationInfo n) $ LocalBindDeclaration ns Nothing
parsePatBind _ = Nothing

parseDerivingDecl :: SrcInfo t => Decl t -> Maybe Declaration
parseDerivingDecl (DerivDecl _ _ r)
    = Just $ ModifierDeclaration (DeclarationInfo $ Symbol $ prettyPrint r) 
           $ DerivingDeclaration cls ts
  where
    (cls:ts) = findName r
parseDerivingDecl _ = Nothing

--maybeFirst :: (a -> Maybe b) -> [a] -> Maybe b
--maybeFirst f xs = (sequence $ map f xs) >>= uncons >>= return . fst

-- | Class of types which can yield a list of symbols
class HasNames a where
    findName :: a -> [Symbol]

-- | 
instance SrcInfo a => HasNames (InstRule a) where
    findName (IRule _ _ _ h) = findName h
    findName (IParen _ h) = findName h

-- | 
instance SrcInfo a => HasNames (InstHead a) where
    findName (IHCon _ n) = [toSym n]
    findName (IHInfix _ t n) = [toSym t, toSym n]
    findName (IHParen _ h) = findName h
    findName (IHApp _ h t) = toSym t : findName h

-- | 
instance HasNames (Pat a) where
    findName (PVar _ n) = [toSym n]
    findName (PNPlusK _ n _) = [toSym n]
    findName (PInfixApp _ p1 _ p2) = findName p1 ++ findName p2
    findName (PApp _ _ ps) = concatMap findName ps
    findName (PTuple _ _ ps) = concatMap findName ps
    findName (PList _ ps) = concatMap findName ps
    findName (PRec _ _ fs) = concatMap findName fs
    findName (PAsPat _ n _) = [toSym n]
    findName (PIrrPat _ p) = findName p
    findName (PatTypeSig _ p _) = findName p
    findName _ = [] -- TODO: rest

-- | 
instance HasNames (PatField l) where
    findName (PFieldPat _ _ p) = findName p
    findName (PFieldPun _ n) = [toSym n]
    findName _ = [] -- TODO: rest


-- | Try to convert a declaration
tryConvert :: SrcInfo t => Decl t -> Maybe Declaration
tryConvert x
    = getFirst 
    $ mconcat
    $ map (First . ($x))
        [ parseTypeSynonym
        , parseDataDecl
        , parseNewtypeDecl
        , parseFuncBind
        , parseClassDecl
        , parseTypeSignature
        , parsePatBind
        , parseDerivingDecl
        , parseInstanceDecl
        , parseGADTDecl
        , parseGADTNewtypeDecl
        ]
  
-- | Parse a string containing 0 or more delcarations
parseMany :: String -> Either (SolutionError u) [Declaration]
parseMany s = case parseModule s of
    ParseOk (Syntax.Module _ _ _ _ ds) -> case mapM tryConvert ds of
        Just y -> Right y
        Nothing -> Left $ Unsupported ""
    ParseOk _ -> Left $ Unsupported "" 
    ParseFailed l msg -> Left $ ParseError l msg ""

-- | 
instance Spannable Comment where
    getSpan (Comment _ s _) = s

-- | Convert a declaration and extract its body
convertWithBody :: (Show a, Spannable a, SrcInfo a) 
                => String 
                -> [Comment]
                -> Decl a
                -> Either (SolutionError u) (WithBody Declaration)
convertWithBody str cs x = case tryConvert x of
    Just decl -> Right $ WithBody decl (spanWithComments >< str)
      where
        leftBoundaryComments = leftBoundaries str (ann x) cs 
        spanWithComments = case leftBoundaryComments of
            [] -> getSpan $ ann x
            (c:_) -> mergeSrcSpan (getSpan c) (getSpan $ ann x)
        
    Nothing -> Left $ Unsupported $ (ann x >< str) ++ " " ++ show (ann x)

-- | Take a list of declarations and combine the first type signature with the first bind
-- This function assumes that all declarations in the input list have the same symbol
combineFuncAndTypeSig :: [WithBody Declaration] -> [WithBody Declaration]
combineFuncAndTypeSig ds = case (typeSigs,funcBinds) of
    (WithBody sig sb:_,WithBody func fb:_) -> WithBody newDecl newBody : notFuncBinds
      where
        ModifierDeclaration _ (TypeSignatureDeclaration _ type_) = sig
        BindDeclaration info (LocalBindDeclaration syms _) = func
        newDecl = BindDeclaration info (LocalBindDeclaration syms (Just type_)) 
        newBody = sb ++ "\n" ++ fb
    _ -> ds
  where
    (typeSigs,notTypeSigs) = partition (isTypeSig . item) ds
    isTypeSig (ModifierDeclaration _ (TypeSignatureDeclaration _ _)) = True
    isTypeSig _ = False
    (funcBinds,notFuncBinds) = partition (isFuncBind . item) notTypeSigs
    isFuncBind (BindDeclaration _ (LocalBindDeclaration _ _)) = True
    isFuncBind _ = False

-- | Parse a declaration
parse :: String -> Either (SolutionError u) Declaration
parse s = case parseDecl s of
    ParseOk x -> case tryConvert x of
        Just y -> Right y
        Nothing -> Left $ Unsupported $ show x
    ParseFailed l msg -> Left $ ParseError l msg ""


-- |Take a string and produce the needed information for building a Module
parseWithBody :: String -> Maybe FilePath -> Either (SolutionError u) [WithBody Declaration]
parseWithBody s p = case parseModuleWithComments parseMode s of
    ParseOk (Syntax.Module _ Nothing [] [] ds, cs) -> mapM (convertWithBody s cs) ds
    ParseOk (XmlPage{}, _) -> Left $ Unsupported "Xml pages are not yet supported"
    ParseOk (XmlHybrid{}, _) -> Left $ Unsupported "Xml pages are not yet supported"
    ParseOk (Syntax.Module{}, _) -> Left $ InvalidOperation 
        "Found module head when trying to parse a declaration, use \"Module.parse\" instead" 
        "Declaration.parseWithBody"
    ParseFailed l msg -> Left $ ParseError l msg ""
  where
    parseMode = case p of
        Just fn -> defaultParseMode{parseFilename=fn,extensions=exts,fixities=Just[]}
        Nothing -> defaultParseMode{extensions=exts,fixities=Just[]}
    exts = EnableExtension LambdaCase : glasgowExts
