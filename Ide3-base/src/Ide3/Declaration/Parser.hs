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
import Language.Haskell.Exts.Annotated.Syntax hiding (Symbol, Ann, ann)
import qualified Language.Haskell.Exts.Annotated.Syntax as Syntax
import Language.Haskell.Exts.SrcLoc (SrcInfo)
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

import Ide3.Utils.Parser
import Ide3.SrcLoc
import Ide3.SrcLoc.Exts ()

import Ide3.Types.Internal
import qualified Ide3.Constructor as Constructor
import qualified Ide3.Constructor.Exts as Constructor

-- | Convert a declaration if it is a type synonym
parseTypeSynonym :: (Spannable t, SrcInfo t) => Decl t -> Maybe Declaration
parseTypeSynonym (TypeDecl _ h t)
    = Just $ TypeDeclaration (DeclarationInfo (toSym h))
                             (TypeSynonym (toSym h)
                                          (toSym t)
                             )
parseTypeSynonym _ = Nothing

-- | Convert a declaration if it is a GADT newtype
parseGADTNewtypeDecl :: (Spannable t, SrcInfo t) => Decl t -> Maybe Declaration
parseGADTNewtypeDecl (GDataDecl _ (NewType _) _ h _ [con] _)
    = Just $ TypeDeclaration (DeclarationInfo (toSym h))
                             (NewtypeDeclaration (toSym h)
                                              (Constructor.toConstructor con)
                             )
parseGADTNewtypeDecl _ = Nothing

-- | Convert a declaration if it is a newtype declaration
parseNewtypeDecl :: (Spannable t, SrcInfo t) => Decl t -> Maybe Declaration
parseNewtypeDecl (DataDecl _ (NewType _) _ h [con] _)
    = Just $ TypeDeclaration (DeclarationInfo (toSym h))
                             (NewtypeDeclaration (toSym h)
                                                 (Constructor.toConstructor con)
                             )
parseNewtypeDecl _ = Nothing

-- | Convert a declaration if it is a GADT data
parseGADTDecl :: (Spannable t, SrcInfo t) => Decl t -> Maybe Declaration
parseGADTDecl (GDataDecl _ (DataType _) _ h _ cons _)
    = Just $ TypeDeclaration (DeclarationInfo (toSym h))
                             (DataDeclaration (toSym h)
                                              (map Constructor.toConstructor cons)
                             )
parseGADTDecl _ = Nothing

-- | Convert a declaration if it is a data declaration
parseDataDecl :: (Spannable t, SrcInfo t) => Decl t -> Maybe Declaration
parseDataDecl (DataDecl _ (DataType _) _ h cons _)
    = Just $ TypeDeclaration (DeclarationInfo (toSym h))
                             (DataDeclaration (toSym h)
                                              (map Constructor.toConstructor cons)
                             )
parseDataDecl _ = Nothing

-- | Convert a declaration if it is a function bind
parseFuncBind :: (Spannable t, SrcInfo t) => Decl t -> Maybe Declaration
parseFuncBind (FunBind _ (m:_)) = Just $ BindDeclaration (DeclarationInfo $ toSym n) 
                                     $ LocalBindDeclaration [toSym n] Nothing
  where
    n = case m of
        Match _ n' _ _ _ -> n'
        InfixMatch _ _ n' _ _ _ -> n'
parseFuncBind _ = Nothing

-- | Convert a declaration if it is a type signature
parseTypeSignature :: (Spannable t, SrcInfo t) => Decl t -> Maybe Declaration
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
parseClassDecl :: (Spannable t, SrcInfo t) => Decl t -> Maybe Declaration
parseClassDecl (ClassDecl _ _ h _ ds)
    = Just $ TypeDeclaration (DeclarationInfo $ toSym h)
           $ ClassDeclaration (toSym h) ds'
  where
    parseSubDecl (ClsDecl _ d) = tryConvert d
    parseSubDecl _ = Nothing
    Just ds' = ds >>= mapM parseSubDecl
parseClassDecl _ = Nothing

-- | Convert a declaration if it is an instance declaration
parseInstanceDecl :: (Spannable t, SrcInfo t) => Decl t -> Maybe Declaration
parseInstanceDecl (InstDecl _ _ r _)
    = Just $ ModifierDeclaration (DeclarationInfo $ Symbol $ prettyPrint r) 
           $ InstanceDeclaration cls ts []
  where
    (cls:ts) = findName r
parseInstanceDecl _ = Nothing

-- | Convert a declaration if it is a pattern bind
parsePatBind :: (Spannable t, SrcInfo t) => Decl t -> Maybe Declaration
parsePatBind (PatBind _ p _ _) = case findName p of
    [] -> Nothing
    ns@(n:_) -> Just $ BindDeclaration (DeclarationInfo n) $ LocalBindDeclaration ns Nothing
parsePatBind _ = Nothing

-- | Convert a declaration if it is a standalone deriving declaration
parseDerivingDecl :: (Spannable t, SrcInfo t) => Decl t -> Maybe Declaration
parseDerivingDecl (DerivDecl _ _ r)
    = Just $ ModifierDeclaration (DeclarationInfo $ Symbol $ prettyPrint r) 
           $ DerivingDeclaration cls ts
  where
    (cls:ts) = findName r
parseDerivingDecl _ = Nothing

--maybeFirst :: (a -> Maybe b) -> [a] -> Maybe b
--maybeFirst f xs = (sequence $ map f xs) >>= uncons >>= return . fst

-- | Try to convert a declaration
tryConvert :: (Spannable t, SrcInfo t) => Decl t -> Maybe Declaration
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
    ParseFailed l msg -> Left $ ParseError (toSrcFileLoc l) msg ""


-- | Convert a declaration and extract its body, along with the comments
--  directly above it
convertWithBody :: (FileSpannable l, SrcInfo l)
                => String 
                -> [Comment]
                -> Decl l
                -> Either (SolutionError u) (Ann SrcSpan (WithBody Declaration))
convertWithBody str cs x = case tryConvert x of
    Just decl -> Right $ Ann finalSpan $ WithBody decl $ finalSpan >< str
      where
        leftBoundaryComments sp = case leftBoundaries str sp cs of
            [] -> sp
            (c:_) -> leftBoundaryComments $ mergeSrcSpan (toSrcSpan c) sp
        -- This is here because of a bug in the haskell-src-exts library
        -- that causes the sourece spans reported for instance declarations 
        -- to contain the whitespace and comments up to the next declaration.
        -- By finding comments which "intersect" with the declaration, we can
        -- remove the offending span section
        noIntersectors sp = case intersectors str sp cs of
            [] -> sp
            (c:_) -> sp `subtractSrcSpan` c $ str
        finalSpan = leftBoundaryComments $ noIntersectors $ toSrcSpan $ Syntax.ann x
    Nothing -> Left $ Unsupported $ (Syntax.ann x >< str) ++ " " ++ show (toSrcFileSpan $ Syntax.ann x)

-- | Take a list of declarations and combine the first type signature with the first bind
-- This function assumes that all declarations in the input list have the same symbol
combineFuncAndTypeSig :: [Ann SrcSpan (WithBody Declaration)] -> [Ann SrcSpan (WithBody Declaration)]
combineFuncAndTypeSig ds = case (typeSigs,funcBinds) of
    (Ann l (WithBody sig sb) : _, Ann l' (WithBody func fb) : _) 
        -> (Ann (l `mergeSrcSpan` l') $ WithBody newDecl newBody) : notFuncBinds
      where
        ModifierDeclaration _ (TypeSignatureDeclaration _ type_) = sig
        BindDeclaration info (LocalBindDeclaration syms _) = func
        newDecl = BindDeclaration info (LocalBindDeclaration syms (Just type_)) 
        newBody = sb ++ "\n" ++ fb
    _ -> ds
  where
    (typeSigs,notTypeSigs) = partition (isTypeSig . item . unAnn) ds
    isTypeSig (ModifierDeclaration _ (TypeSignatureDeclaration _ _)) = True
    isTypeSig _ = False
    (funcBinds,notFuncBinds) = partition (isFuncBind . item . unAnn) notTypeSigs
    isFuncBind (BindDeclaration _ (LocalBindDeclaration _ _)) = True
    isFuncBind _ = False

-- | Parse a declaration
parse :: String -> Either (SolutionError u) Declaration
parse s = case parseDecl s of
    ParseOk x -> case tryConvert x of
        Just y -> Right y
        Nothing -> Left $ Unsupported $ show x
    ParseFailed l msg -> Left $ ParseError (toSrcFileLoc l) msg ""


-- |Take a string and produce the needed information for building a Module
parseWithBody :: String -> Maybe FilePath -> Either (SolutionError u) [Ann SrcSpan (WithBody Declaration)]
parseWithBody s p = case parseModuleWithComments parseMode s of
    ParseOk (Syntax.Module _ Nothing [] [] ds, cs) -> mapM (convertWithBody s cs) ds
    ParseOk (XmlPage{}, _) -> Left $ Unsupported "Xml pages are not yet supported"
    ParseOk (XmlHybrid{}, _) -> Left $ Unsupported "Xml pages are not yet supported"
    ParseOk (Syntax.Module{}, _) -> Left $ InvalidOperation 
        "Found module head when trying to parse a declaration, use \"Module.parse\" instead" 
        "Declaration.parseWithBody"
    ParseFailed l msg -> Left $ ParseError (toSrcFileLoc l) msg ""
  where
    parseMode = case p of
        Just fn -> defaultParseMode{parseFilename=fn,extensions=exts,fixities=Just[]}
        Nothing -> defaultParseMode{extensions=exts,fixities=Just[]}
    exts = EnableExtension LambdaCase : glasgowExts
