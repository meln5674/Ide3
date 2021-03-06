{-|
Module      : Ide3.Declaration
Description : Operations on declarations
Copyright   : (c) Andrew Melnick, 2016

License     : BSD3
Maintainer  : meln5674@kettering.edu
Stability   : experimental
Portability : POSIX
-}
module Ide3.Declaration
    ( module Ide3.Declaration
    , Parser.parse
    , Parser.convertWithBody
    ) where

import Data.List

import Data.Text (Text)

import qualified Ide3.OrderedMap as OMap

import Ide3.Types.Internal
import qualified Ide3.Declaration.Parser as Parser

import qualified Ide3.Declaration.TypeDeclaration as TypeDeclaration
import qualified Ide3.Declaration.BindDeclaration as BindDeclaration
import qualified Ide3.Declaration.ModifierDeclaration as ModifierDeclaration

import Ide3.SrcLoc.Types

import Ide3.Utils.Parser


-- | Parse a string containing either a single declaration or multiple
-- declarations which can be combined into a single declaration
parseAndCombine :: Text
                -> Maybe FilePath 
                -> Either (SolutionError u) (WithBody Declaration)
parseAndCombine s fp = do 
    ds <- Parser.parseWithBody s fp
    let combined = combineMany ds
    case combined of
        [d] -> return $ unAnn d
        [] -> Left $ InvalidOperation "Found no declarations" 
                                      "Declaration.parseAndCombine"
        _ -> Left $ InvalidOperation "Found more than 1 declaration"
                                     "Declaration.parseAndCombine"
    
-- | Parse a declaration, but if it fails, return an unparseable declaration
-- along with the error
parseAndCombineLenient :: Text -> Maybe FilePath -> DeclarationInfo 
             -> Either (WithBody Declaration, SolutionError u) 
                       (WithBody Declaration)
parseAndCombineLenient s p di = case parseAndCombine s p of
    Right decl -> Right decl
    Left err -> Left (WithBody (UnparseableDeclaration di) s, err)

-- | Find declarations that can be merged and merge them
combineMany :: [Ann SrcSpan (WithBody Declaration)] 
            -> [Ann SrcSpan (WithBody Declaration)]
combineMany ds = ds'
  where
    m = OMap.partitionBy (info . item . unAnn) ds
    m' = OMap.map Parser.combineFuncAndTypeSig m
    ds' = concat $ OMap.elems m'

-- | Get the identifying information from a declaration
info :: Declaration -> DeclarationInfo
info (TypeDeclaration i _) = i
info (BindDeclaration i _) = i
info (ModifierDeclaration i _) = i
info (SpliceDeclaration i _) = i
info (UnparseableDeclaration i) = i

-- | Get a a list of all symbols provided by a declaration
symbolsProvided :: Declaration -> [Symbol]
symbolsProvided t = typesProvided t 
                 ++ constructorsProvided t
                 ++ bindsProvided t

-- | Test if a declaration provides a symbol
providesSymbol :: Declaration -> Symbol -> Bool
d `providesSymbol` s = s `elem` symbolsProvided d

-- | If a declaration provides a symbol, get all of the other symbols it
-- provides
otherSymbols :: Declaration -> Symbol -> Maybe [Symbol]
otherSymbols d s | d `providesSymbol` s = Just $ delete s $ symbolsProvided d
                 | otherwise = Nothing

-- | Get a list of types a declaration provides
typesProvided :: Declaration -> [Symbol]
typesProvided (TypeDeclaration _ t) = [TypeDeclaration.typeCreated t]
typesProvided _ = []

-- | Get a list of constructors a declaration provides
constructorsProvided :: Declaration -> [Symbol]
constructorsProvided (TypeDeclaration _ t)
    = TypeDeclaration.constructorsCreated t
constructorsProvided _ = []

-- | Get a list of binds a declaration provides
bindsProvided :: Declaration -> [Symbol]
bindsProvided (BindDeclaration _ t) = BindDeclaration.symbolsCreated t
bindsProvided (TypeDeclaration _ t) = TypeDeclaration.bindsCreated t
bindsProvided _ = []

-- | Get a list of symbols a declaration affects
symbolsAffected :: Declaration -> [Symbol]
symbolsAffected (ModifierDeclaration _ t)
    = ModifierDeclaration.symbolsAffected t
symbolsAffected _ = []

-- | Test if a declaration affects a symbol
affectsSymbol :: Declaration -> Symbol -> Bool
d `affectsSymbol` s = s `elem` symbolsAffected d
