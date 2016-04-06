module Ide3.Types where

import Language.Haskell.Exts.Pretty

import Language.Haskell.Exts.Syntax hiding (Symbol, Module, Type)
import qualified Language.Haskell.Exts.Syntax as Syntax


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


data ModuleInfo = ModuleInfo Symbol
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

instance ToSym Name where
    toSym (Ident n) = Symbol n
    toSym (Syntax.Symbol n) = Symbol n

instance ToSym CName where
    toSym (VarName n) = toSym n
    toSym (ConName n) = toSym n

instance ToSym ModuleName where
    toSym (ModuleName n) = Symbol n

instance ToSym SpecialCon where
    toSym UnitCon = Symbol "()"
    toSym ListCon = Symbol "[]"
    toSym FunCon = Symbol "->"
    toSym (TupleCon Unboxed n) = Symbol $ "(" ++ replicate n ',' ++ ")"
    toSym (TupleCon Boxed n) = Symbol $ "(#" ++ replicate n ',' ++ "#)"
    toSym Cons = Symbol ":"
    toSym UnboxedSingleCon = Symbol "(# #)"

instance ToSym QName where
    toSym (Qual m n) = toSym m `joinSym` toSym n
    toSym (UnQual n) = toSym n
    toSym (Special s) = toSym s

instance ToSym Syntax.Type where
    --toSym _ = Symbol $ "There's a type here, I promise" --TODO
    toSym = Symbol . prettyPrint

type ProjectError = String
