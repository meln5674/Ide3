module Ide3.Types where

import Language.Haskell.Exts.Pretty

import Language.Haskell.Exts.Annotated.Syntax hiding (Symbol, Module, Type)
import qualified Language.Haskell.Exts.Annotated.Syntax as Syntax
import Language.Haskell.Exts.SrcLoc

import qualified Data.Map as Map
import Data.Map.Strict ( Map )

data WithBody a = WithBody a String
    deriving (Show, Read, Eq)

body :: WithBody a -> String
body (WithBody _ s) = s

bodies :: [WithBody a] -> [String]
bodies = map body

item :: WithBody a -> a
item (WithBody x _) = x

items :: [WithBody a] -> [a]
items = map item

instance Functor WithBody where
    fmap f (WithBody x s) = WithBody (f x) s
    

newtype Symbol = Symbol String
    deriving (Show, Read, Eq, Ord)

joinSym :: Symbol -> Symbol -> Symbol
joinSym = undefined

data ProjectInfo = ProjectInfo
    deriving (Show, Read, Eq)
data BuildInfo = BuildInfo
    deriving (Show, Read, Eq)


data Project = Project ProjectInfo (Map ModuleInfo Module) BuildInfo
    deriving (Show, Read, Eq)


data ModuleInfo 
    = ModuleInfo Symbol
    | UnamedModule (Maybe FilePath)
    deriving (Show, Read, Eq, Ord)

getModuleName :: ModuleInfo -> Symbol
getModuleName (ModuleInfo n) = n

data Module = Module ModuleInfo 
                     (Map ImportId (WithBody Import))
                     (Maybe (Map ExportId (WithBody Export)))
                     (Map DeclarationInfo (WithBody Declaration))
    deriving (Show, Read, Eq)

data ModuleChild a = ModuleChild ModuleInfo a
    deriving (Show, Eq, Ord)

getChild :: ModuleChild a -> a
getChild (ModuleChild _ a) = a

withChildF :: Functor f => (a -> b) -> ModuleChild (f a) -> ModuleChild (f b)
withChildF f (ModuleChild mi x) = ModuleChild mi (f <$> x)

instance Functor ModuleChild where
    fmap f (ModuleChild mi x) = ModuleChild mi (f x)

data Import
    = ModuleImport Symbol Bool (Maybe Symbol)
    | WhitelistImport Symbol Bool (Maybe Symbol) [ImportKind]
    | BlacklistImport Symbol Bool (Maybe Symbol) [ImportKind]
    deriving (Show, Read, Eq)

type ImportId = Int

data ImportKind
    = NameImport Symbol
    | AbsImport Symbol Symbol
    | AllImport Symbol
    | SomeImport Symbol [Symbol]
    deriving (Show, Read, Eq)

data Export
    = SingleExport Symbol
    | ModuleExport Symbol
    | AggregateExport Symbol (Maybe [Symbol])
    deriving (Show, Read, Eq)

type ExportId = Int

data DeclarationInfo = DeclarationInfo Symbol
    deriving (Show, Read, Eq, Ord)

data Declaration
    = TypeDeclaration DeclarationInfo TypeDeclaration
    | BindDeclaration DeclarationInfo BindDeclaration
    | ModifierDeclaration DeclarationInfo ModifierDeclaration
    deriving (Show, Read, Eq)



data TypeDeclaration
    = ClassDeclaration Symbol [Declaration]
    | TypeSynonym Symbol Symbol
    | DataDeclaration Symbol [Constructor]
    | NewtypeDeclaration Symbol Constructor
    deriving (Show, Read, Eq)


    
data ForeignInfo = ForeignInfo
    deriving (Show, Read, Eq)

data BindDeclaration
    = LocalBindDeclaration [Symbol] (Maybe Symbol)
    | ForeignBindDeclaration Symbol Type ForeignInfo
    deriving (Show, Read, Eq)

data ModifierDeclaration
    = FixityDeclaration [Symbol] Int FixityType
    | InstanceDeclaration Symbol [Symbol] [Declaration]
    | TypeSignatureDeclaration Symbol Symbol
    deriving (Show, Read, Eq)

data FixityType = FixityType
    deriving (Show, Read, Eq)

data Constructor
    = PrefixConstructor Symbol [Symbol]
    | InfixConstructor Symbol Symbol Symbol
    | RecordConstructor Symbol [(Symbol, Symbol)]
    deriving (Show, Read, Eq)


data Type = Type
    deriving (Show, Read, Eq)

data TypeSearchResult
    = TypeFound Type
    | TypeNotGiven
    | IsNotBindDecl

class ToSym a where
    toSym :: a -> Symbol

instance ToSym (Name a) where
    toSym (Ident _ n)         = Symbol n
    toSym (Syntax.Symbol _ n) = Symbol n

instance ToSym (CName a) where
    toSym (VarName _ n) = toSym n
    toSym (ConName _ n) = toSym n

instance ToSym (ModuleName a) where
    toSym (ModuleName _ n) = Symbol n

instance ToSym (SpecialCon a) where
    toSym (UnitCon _)   = Symbol "()"
    toSym (ListCon _)   = Symbol "[]"
    toSym (FunCon _)    = Symbol "->"
    toSym (TupleCon _ Unboxed n) = Symbol $ "(" ++ replicate n ',' ++ ")"
    toSym (TupleCon _ Boxed n) = Symbol $ "(#" ++ replicate n ',' ++ "#)"
    toSym (Cons _) = Symbol ":"
    toSym (UnboxedSingleCon _) = Symbol "(# #)"

instance ToSym (QName a) where
    toSym (Qual _ m n) = toSym m `joinSym` toSym n
    toSym (UnQual _ n) = toSym n
    toSym (Special _ s) = toSym s

instance SrcInfo a => ToSym (Syntax.Type a) where
    toSym = Symbol . prettyPrint

instance ToSym (DeclHead a) where
    toSym (DHead _ n) = toSym n
    toSym (DHInfix _ _ n) = toSym n
    toSym (DHParen _ h) = toSym h
    toSym (DHApp _ h _) = toSym h


type ProjectError = String
