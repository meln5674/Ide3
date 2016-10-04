module SyntaxHighlighter2 where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Except

import qualified Language.Haskell.Exts.Lexer as Lex
import Language.Haskell.Exts.Parser

import Language.Haskell.Exts.SrcLoc

--import Graphics.UI.Gtk

import Ide3.Types (SolutionResult, SolutionError (..))

import Ide3.SrcLoc
import Ide3.SrcLoc.Exts

import GuiClass.Types

data SyntaxComponent
    = Comment
    | VarId
    | VarSym
    | ConId
    | ConSym
    | Syntax
    | Keyword
    | Pragma
    | Literal
  deriving (Eq,Ord,Enum,Read,Show)

allSyntaxComponents :: [SyntaxComponent]
allSyntaxComponents = [Comment .. Literal]

data HighlightInst = HighlightInst SyntaxComponent CursorPosition CursorPosition

classifyToken :: Lex.Token -> SyntaxComponent
classifyToken Lex.VarId{}               = VarId
classifyToken Lex.QVarId{}              = VarId
classifyToken Lex.IDupVarId{}           = VarId
classifyToken Lex.ILinVarId{}           = VarId
classifyToken Lex.QConId{}              = ConId
classifyToken Lex.ConId{}               = ConId
classifyToken Lex.DVarId{}              = VarId
classifyToken Lex.VarSym{}              = VarSym
classifyToken Lex.ConSym{}              = ConSym
classifyToken Lex.QVarSym{}             = VarSym
classifyToken Lex.QConSym{}             = ConSym
classifyToken Lex.IntTok{}              = Literal
classifyToken Lex.Character{}           = Literal
classifyToken Lex.FloatTok{}            = Literal
classifyToken Lex.StringTok{}           = Literal
classifyToken Lex.IntTokHash{}          = Literal
classifyToken Lex.WordTokHash{}         = Literal
classifyToken Lex.FloatTokHash{}        = Literal
classifyToken Lex.DoubleTokHash{}       = Literal
classifyToken Lex.CharacterHash{}       = Literal
classifyToken Lex.StringHash{}          = Literal
classifyToken Lex.LeftParen{}           = Syntax
classifyToken Lex.RightParen{}          = Syntax
classifyToken Lex.LeftHashParen{}       = Syntax
classifyToken Lex.RightHashParen{}      = Syntax
classifyToken Lex.SemiColon{}           = Syntax
classifyToken Lex.LeftCurly{}           = Syntax
classifyToken Lex.RightCurly{}          = Syntax
classifyToken Lex.VRightCurly{}         = Syntax
classifyToken Lex.LeftSquare{}          = Syntax
classifyToken Lex.RightSquare{}         = Syntax
classifyToken Lex.ParArrayLeftSquare{}  = Syntax
classifyToken Lex.ParArrayRightSquare{} = Syntax
classifyToken Lex.Comma{}               = Syntax
classifyToken Lex.Underscore{}          = Syntax
classifyToken Lex.BackQuote{}           = Syntax
classifyToken Lex.Dot{}                 = Syntax
classifyToken Lex.DotDot{}              = Syntax
classifyToken Lex.Colon{}               = Syntax
classifyToken Lex.QuoteColon{}          = Syntax
classifyToken Lex.DoubleColon{}         = Syntax
classifyToken Lex.Equals{}              = Syntax
classifyToken Lex.Backslash{}           = Syntax
classifyToken Lex.Bar{}                 = Syntax
classifyToken Lex.LeftArrow{}           = Syntax
classifyToken Lex.RightArrow{}          = Syntax
classifyToken Lex.At{}                  = Syntax
classifyToken Lex.Tilde{}               = Syntax
classifyToken Lex.DoubleArrow{}         = Syntax
classifyToken Lex.Minus{}               = VarSym
classifyToken Lex.Exclamation{}         = Syntax
classifyToken Lex.Star{}                = VarSym
classifyToken Lex.LeftArrowTail{}       = Syntax
classifyToken Lex.RightArrowTail{}      = Syntax
classifyToken Lex.LeftDblArrowTail{}    = Syntax
classifyToken Lex.RightDblArrowTail{}   = Syntax
classifyToken Lex.THExpQuote{}          = Syntax
classifyToken Lex.THPatQuote{}          = Syntax
classifyToken Lex.THDecQuote{}          = Syntax
classifyToken Lex.THTypQuote{}          = Syntax
classifyToken Lex.THCloseQuote{}        = Syntax
classifyToken Lex.THIdEscape{}          = Syntax
classifyToken Lex.THParenEscape{}       = Syntax
classifyToken Lex.THVarQuote{}          = Syntax
classifyToken Lex.THTyQuote{}           = Syntax
classifyToken Lex.THQuasiQuote{}        = Syntax
classifyToken Lex.RPGuardOpen{}         = Syntax
classifyToken Lex.RPGuardClose{}        = Syntax
classifyToken Lex.RPCAt{}               = Syntax
classifyToken Lex.XCodeTagOpen{}        = Syntax
classifyToken Lex.XCodeTagClose{}       = Syntax
classifyToken Lex.XStdTagOpen{}         = Syntax
classifyToken Lex.XStdTagClose{}        = Syntax
classifyToken Lex.XCloseTagOpen{}       = Syntax
classifyToken Lex.XEmptyTagClose{}      = Syntax
classifyToken Lex.XChildTagOpen{}       = Syntax
classifyToken Lex.XPCDATA{}             = Syntax
classifyToken Lex.XRPatOpen{}           = Syntax
classifyToken Lex.XRPatClose{}          = Syntax
classifyToken Lex.PragmaEnd{}           = Pragma
classifyToken Lex.RULES{}               = Pragma
classifyToken Lex.INLINE{}              = Pragma
classifyToken Lex.INLINE_CONLIKE{}      = Pragma
classifyToken Lex.SPECIALISE{}          = Pragma
classifyToken Lex.SPECIALISE_INLINE{}   = Pragma
classifyToken Lex.SOURCE{}              = Pragma
classifyToken Lex.DEPRECATED{}          = Pragma
classifyToken Lex.WARNING{}             = Pragma
classifyToken Lex.SCC{}                 = Pragma
classifyToken Lex.GENERATED{}           = Pragma
classifyToken Lex.CORE{}                = Pragma
classifyToken Lex.UNPACK{}              = Pragma
classifyToken Lex.OPTIONS{}             = Pragma
classifyToken Lex.LANGUAGE{}            = Pragma
classifyToken Lex.ANN{}                 = Pragma
classifyToken Lex.MINIMAL{}             = Pragma
classifyToken Lex.NO_OVERLAP{}          = Pragma
classifyToken Lex.OVERLAP{}             = Pragma
classifyToken Lex.INCOHERENT{}          = Pragma
classifyToken Lex.KW_As{}               = Keyword
classifyToken Lex.KW_By{}               = Keyword
classifyToken Lex.KW_Case{}             = Keyword
classifyToken Lex.KW_Class{}            = Keyword
classifyToken Lex.KW_Data{}             = Keyword
classifyToken Lex.KW_Default{}          = Keyword
classifyToken Lex.KW_Deriving{}         = Keyword
classifyToken Lex.KW_Do{}               = Keyword
classifyToken Lex.KW_MDo{}              = Keyword
classifyToken Lex.KW_Else{}             = Keyword
classifyToken Lex.KW_Family{}           = Keyword
classifyToken Lex.KW_Forall{}           = Keyword
classifyToken Lex.KW_Group{}            = Keyword
classifyToken Lex.KW_Hiding{}           = Keyword
classifyToken Lex.KW_If{}               = Keyword
classifyToken Lex.KW_Import{}           = Keyword
classifyToken Lex.KW_In{}               = Keyword
classifyToken Lex.KW_Infix{}            = Keyword
classifyToken Lex.KW_InfixL{}           = Keyword
classifyToken Lex.KW_InfixR{}           = Keyword
classifyToken Lex.KW_Instance{}         = Keyword
classifyToken Lex.KW_Let{}              = Keyword
classifyToken Lex.KW_Module{}           = Keyword
classifyToken Lex.KW_NewType{}          = Keyword
classifyToken Lex.KW_Of{}               = Keyword
classifyToken Lex.KW_Proc{}             = Keyword
classifyToken Lex.KW_Rec{}              = Keyword
classifyToken Lex.KW_Role{}             = Keyword
classifyToken Lex.KW_Then{}             = Keyword
classifyToken Lex.KW_Type{}             = Keyword
classifyToken Lex.KW_Using{}            = Keyword
classifyToken Lex.KW_Where{}            = Keyword
classifyToken Lex.KW_Qualified{}        = Keyword
classifyToken Lex.KW_Pattern{}          = Keyword
classifyToken Lex.KW_Foreign{}          = Keyword
classifyToken Lex.KW_Export{}           = Keyword
classifyToken Lex.KW_Safe{}             = Keyword
classifyToken Lex.KW_Unsafe{}           = Keyword
classifyToken Lex.KW_Threadsafe{}       = Keyword
classifyToken Lex.KW_Interruptible{}    = Keyword
classifyToken Lex.KW_StdCall{}          = Keyword
classifyToken Lex.KW_CCall{}            = Keyword
classifyToken Lex.KW_CPlusPlus{}        = Keyword
classifyToken Lex.KW_DotNet{}           = Keyword
classifyToken Lex.KW_Jvm{}              = Keyword
classifyToken Lex.KW_Js{}               = Keyword
classifyToken Lex.KW_JavaScript{}       = Keyword
classifyToken Lex.KW_CApi{}             = Keyword
classifyToken Lex.EOF{}                 = Syntax


getHighlights :: (Monad m) 
              => String 
              -> SolutionResult u m [HighlightInst]
getHighlights text = case Lex.lexTokenStream text of
    ParseOk toks -> forM toks $ \Loc{loc=loc,unLoc=tok} -> lift $ do
        let start = let (s,e) = srcSpanStart loc in (Row $ s-1,Column $ e-1)
        let end = let (s,e) = srcSpanEnd loc in (Row $ s-1,Column $ e-1)
        let tok' = classifyToken tok
        return $ HighlightInst tok' start end
    ParseFailed loc err -> throwE $ ParseError (SrcFileLoc "" $ toSrcLoc loc) err ""
