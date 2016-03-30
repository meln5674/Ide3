module Ide3.Types where

import qualified Data.Map as Map
import Data.Map.Strict ( Map )

data WithBody a = WithBody a String
    deriving (Show, Read, Eq)

body :: WithBody a -> String
body (WithBody _ s) = s

item :: WithBody a -> a
item (WithBody x _) = x

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
                     [WithBody Import] 
                     (Maybe [WithBody Export])
                     (Map DeclarationInfo (WithBody Declaration))
    deriving (Show, Read, Eq)

data ModuleChild a = ModuleChild ModuleInfo a

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

data ImportKind
    = NameImport Symbol
    | AbsImport Symbol Symbol
    | AllImport Symbol
    | SomeImport Symbol [Symbol]
    deriving (Show, Read, Eq)

data Export
    = SingleExport Symbol
    | ModuleExport Symbol
    | AggregateExport Symbol [Symbol]
    deriving (Show, Read, Eq)

data DeclarationInfo = DeclarationInfo
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
    = LocalBindDeclaration Symbol Type
    | ForeignBindDeclaration Symbol Type ForeignInfo
    deriving (Show, Read, Eq)

data ModifierDeclaration
    = FixityDeclaration [Symbol] Int FixityType
    | InstanceDeclaration Symbol [Symbol] [Declaration]
    deriving (Show, Read, Eq)

data FixityType = FixityType
    deriving (Show, Read, Eq)

data Constructor
    = PrefixConstructor Symbol [Symbol]
    | InfixConstructor Symbol Symbol Symbol
    | RecordConstructor Symbol [(Symbol, Type)]
    deriving (Show, Read, Eq)


data Type = Type
    deriving (Show, Read, Eq)

data TypeSearchResult
    = TypeFound Type
    | TypeNotGiven
    | IsNotBindDecl
