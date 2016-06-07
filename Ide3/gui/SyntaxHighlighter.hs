{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module SyntaxHighlighter where

import Data.Char

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Except

import Graphics.UI.Gtk

import Language.Haskell.Exts.Annotated.Syntax
import Language.Haskell.Exts.Annotated.Parser
import Language.Haskell.Exts.Parser (ParseResult (..))
import Language.Haskell.Exts.SrcLoc hiding (loc)
import Language.Haskell.Exts.Comments

import Ide3.Types (ProjectError (..), ProjectResult)

data Highlightable 
    = VarSym
    | VarId
    | ConSym
    | ConId
    | ConstChar
    | ConstString
    | ConstInt
    | ConstFrac
    | Parenthesis
    | Bracket
    | Curlybracket
    | Keyword
    | SingleComment
    | MultiComment
    deriving (Read,Show,Eq,Enum)

allHighlightables = [VarSym .. MultiComment]

data HighlightInst tag loc
    = HighlightInst tag loc

class Highlight x tag loc where
    highlight :: x -> [HighlightInst tag loc]

instance Highlight (Decl SrcSpanInfo) Highlightable SrcSpanInfo where
    highlight (TypeDecl _ mhead type_) = highlight mhead ++ highlight type_
    highlight _ = [] -- TODO

instance Highlight (DeclHead SrcSpanInfo) Highlightable SrcSpanInfo where
    highlight (DHead _ name) = highlight name
    highlight _ = [] -- TODO

instance Highlight (Type SrcSpanInfo) Highlightable SrcSpanInfo where
    highlight _ = [] -- TODO

instance Highlight (Name SrcSpanInfo) Highlightable SrcSpanInfo where
    highlight (Ident loc (c:_))
        | isUpper c = [HighlightInst ConId loc]
        | otherwise = [HighlightInst VarId loc]
    highlight (Ident loc (':':_)) = [HighlightInst ConSym loc]
    highlight (Ident loc _) = [HighlightInst VarSym loc]

instance Highlight (Module SrcSpanInfo) Highlightable SrcSpanInfo where
    highlight _ = [] -- TODO

instance Highlight Comment Highlightable SrcSpanInfo where
    highlight _ = [] -- TODO

instance (Highlight x tag loc) => Highlight [x] tag loc where
    highlight = concatMap highlight

instance (Highlight x tag loc, Highlight y tag loc) => Highlight (x,y) tag loc where
    highlight (x,y) = highlight x ++ highlight y

getHighlightsInternal :: String -> Either (ProjectError u) [HighlightInst Highlightable SrcSpanInfo]
getHighlightsInternal s = case parseModuleWithComments parseMode s of
    ParseOk m -> Right $ highlight m
    ParseFailed loc msg -> Left $ ParseError loc msg ""
  where
    parseMode = undefined
decodeSrcLocInfo :: SrcSpanInfo -> ((Int,Int),(Int,Int))
decodeSrcLocInfo SrcSpanInfo{srcInfoSpan=l} = (srcSpanStart l, srcSpanEnd l)

getHighlights :: (Monad m) 
              => String
              -> (Highlightable -> m TagName)
              -> (Int -> Int -> m TextIter)
              -> ProjectResult m u [HighlightInst TagName (TextIter,TextIter)]
getHighlights s tagGet locGet = do
    hs <- ExceptT $ return $ getHighlightsInternal s
    lift $ forM hs $ \(HighlightInst tag loc) -> do
        tag' <- tagGet tag
        let ((startR,startC),(endR,endC)) = decodeSrcLocInfo loc
        loc' <- do
            start <- locGet startR startC
            end <- locGet endR endC
            return (start,end)
        return $ HighlightInst tag' loc'
