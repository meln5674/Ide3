{-|
Module      : Ide3.Types
Description : Top level types used by Ide3
Copyright   : (c) Andrew Melnick, 2016

License     : BSD3
Maintainer  : meln5674@kettering.edu
Stability   : experimental
Portability : POSIX

This module contains all of the top level types which are used throughout this
project. The structures here describe a haskell project as a collection of
modules, each of which contain exports, imports, and declarations.
-}
module Ide3.Types where

import Control.Monad.Trans.Except

import Language.Haskell.Exts.Pretty

import Language.Haskell.Exts.Annotated.Syntax hiding (Symbol, Module, Type)
import qualified Language.Haskell.Exts.Annotated.Syntax as Syntax
import Language.Haskell.Exts.SrcLoc

import Text.Printf

import Data.Map.Strict ( Map )

-- |Attaches a string ("body") to another type
data WithBody a = WithBody a String
    deriving (Show, Read, Eq)

-- |Get the body attached to a value
body :: WithBody a -> String
body (WithBody _ s) = s

-- |Get the bodies from a list of values
bodies :: [WithBody a] -> [String]
bodies = map body

-- |Get the item a body is attached to
item :: WithBody a -> a
item (WithBody x _) = x

-- |Get the items from a list of values with bodies attached
items :: [WithBody a] -> [a]
items = map item

instance Functor WithBody where
    -- |Applies the function to the item while leaving the body unchanged
    fmap f (WithBody x s) = WithBody (f x) s
    
-- |Catch-all type for any identifier which is significant to the program
newtype Symbol = Symbol String
    deriving (Show, Read, Eq, Ord)

-- |Join two symbols together such that the second is qualified by the first
joinSym :: Symbol -> Symbol -> Symbol
joinSym (Symbol x) (Symbol y) = Symbol $ x ++ "." ++ y

{-
data SolutionInfo = SolutionInfo

data Solution
    = Solution
    { solutionInfo :: SolutionInfo
    , solutionProjects :: Map ProjectInfo Project
    }
-}
-- |Information about a project
data ProjectInfo = ProjectInfo
    deriving (Show, Read, Eq)
-- |Information on how to build a project
data BuildInfo = BuildInfo
    deriving (Show, Read, Eq)

-- |Top level type, contains information about a project, how to build it,
--  and the modules it contains
data Project
    = Project
    { projectInfo :: ProjectInfo
    , projectModules :: Map ModuleInfo Module
    , projectBuildInfo :: BuildInfo
    , projectExternModules :: Map ModuleInfo ExternModule
    }
    deriving (Show, Read, Eq)

-- |Information identifying a module
data ModuleInfo 
    -- |A module with a name
    = ModuleInfo Symbol              
    -- |An unamed module, possibly with the path it came from    
    | UnamedModule (Maybe FilePath)     
    deriving (Show, Read, Eq, Ord)

-- |Get the name of a module from its info
getModuleName :: ModuleInfo -> Symbol
getModuleName (ModuleInfo n) = n
getModuleName _ = Symbol "UNNAMED MODULE"

-- | A module pragma
type Pragma = String

type ImportCollection = Map ImportId (WithBody Import)

type ExportCollection = Maybe (Map ExportId (WithBody Export))

type DeclarationCollection = Map DeclarationInfo (WithBody Declaration)

-- |A module. 
data Module
    = Module 
    { moduleInfo :: ModuleInfo  -- ^ Identifying information
    , modulePragmas :: [Pragma] -- ^ Pragmas
    , moduleImports :: ImportCollection
    , moduleExports :: ExportCollection
    , moduleDeclarations :: DeclarationCollection
    }
    deriving (Show, Read, Eq)

data ExternExport
    = SingleExternExport Symbol
    | MultiExternExport Symbol [Symbol]
    deriving (Show, Eq, Read, Ord)

-- | A module which is external to the project, only a list of exported symbols
--  are availible
data ExternModule
    = ExternModule
            ModuleInfo
            [ExternExport]
    deriving (Show, Eq, Read, Ord)


-- |A value which is tagged as belonging to a module
data ModuleChild a = ModuleChild ModuleInfo a
    deriving (Show, Eq, Ord)

-- |Get the value tagged with a module
getChild :: ModuleChild a -> a
getChild (ModuleChild _ a) = a

instance Functor ModuleChild where
    fmap f (ModuleChild mi x) = ModuleChild mi (f x)

-- |An import statement. The first three fields of each are:
--  The module being imported
--  Is it qualified?
--  The renamed symbol, if any
data Import
    -- |Importing a module normally
    = ModuleImport Symbol Bool (Maybe Symbol)
    -- |Importing only specific parts of a module
    | WhitelistImport Symbol Bool (Maybe Symbol) [ImportKind]
    -- |Importing everthing but specific parts of a module
    | BlacklistImport Symbol Bool (Maybe Symbol) [ImportKind]
    deriving (Show, Read, Eq)

-- |Identifier for an import statement
type ImportId = Int

-- |A specification from an import list
data ImportKind
    -- |Importing just a symbol
    = NameImport Symbol
    -- |Importing a symbol under a namespace
    | AbsImport Symbol Symbol
    -- |Importing a symbol and either all of some of its sub-symbols
    | AggregateImport Symbol (Maybe [Symbol])
    deriving (Show, Read, Eq)

-- |An export statement
data Export
    -- |Exporting a single symbol
    = SingleExport Symbol
    -- |Exporting an imported module
    | ModuleExport Symbol
    -- |Exporting a symbol and either all or some of its sub-symbols
    | AggregateExport Symbol (Maybe [Symbol])
    deriving (Show, Read, Eq)

-- |Identifier for an export statement
type ExportId = Int

-- |Information identifying a declaration
data DeclarationInfo = DeclarationInfo Symbol
    deriving (Show, Read, Eq, Ord)


-- |A declaration
data Declaration
    -- |A type declaration creates types
    = TypeDeclaration DeclarationInfo TypeDeclaration
    -- |A bind declaration binds names and patterns to expressions
    | BindDeclaration DeclarationInfo BindDeclaration
    -- |A modifier declaration provides some property of an existing declaration
    | ModifierDeclaration DeclarationInfo ModifierDeclaration
    deriving (Show, Read, Eq)


-- |A declaration which provides types
data TypeDeclaration
    -- |Class declaration
    = ClassDeclaration Symbol [Declaration]
    -- |Type synonym
    | TypeSynonym Symbol Symbol
    -- |Data declaration
    | DataDeclaration Symbol [Constructor]
    -- |Newtype declaration
    | NewtypeDeclaration Symbol Constructor
    deriving (Show, Read, Eq)


-- |Data identifying foreign imports and exports
data ForeignInfo = ForeignInfo
    deriving (Show, Read, Eq)

-- |A declaration which binds names and patterns to expressions
data BindDeclaration
    -- |A normal haskell declaration
    = LocalBindDeclaration [Symbol] (Maybe Symbol)
    -- |A declaration which uses the foreign function interface
    | ForeignBindDeclaration Symbol Type ForeignInfo
    deriving (Show, Read, Eq)

-- |A declaration which provides some property about another declaration
data ModifierDeclaration
    -- |A declaration of an operator or infix function's precidence and fixity
    = FixityDeclaration [Symbol] Int FixityType
    -- |A instance of a class
    | InstanceDeclaration Symbol [Symbol] [Declaration]
    -- |A type signature of a bind
    | TypeSignatureDeclaration Symbol Symbol
    deriving (Show, Read, Eq)

-- |The type of fixity of an operator or infix function
data FixityType = FixityType
    deriving (Show, Read, Eq)

-- |A data or newtype constructor
data Constructor
    -- |A constructor of the form C a1 a2 ...
    = PrefixConstructor Symbol [Symbol]
    -- |A constructor of the form a1 C a2
    | InfixConstructor Symbol Symbol Symbol
    -- |A constructor of the form C { a1, a2 :: T1, a3 :: T2, ... }
    | RecordConstructor Symbol [(Symbol, Symbol)]
    deriving (Show, Read, Eq)

-- |A type
data Type = Type
    deriving (Show, Read, Eq)

-- |The result of searching for a type
data TypeSearchResult
    -- |The type was not found
    = TypeFound Type
    -- |The type was not given
    | TypeNotGiven
    -- |The declaration searched was not a bind
    | IsNotBindDecl

-- |Class of types from which a symbol can be extracted
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

-- |Errors that can arrise during modifying and querying a project
data ProjectError u
    = ModuleNotFound ModuleInfo String
    | DeclarationNotFound ModuleInfo DeclarationInfo String
    | SymbolNotFound ModuleInfo Symbol String
    | SymbolNotImported ModuleInfo Symbol String
    | SymbolNotExported ModuleInfo Symbol String
    | NotSubSymbol Symbol Symbol String
    | ModuleNotImported ModuleInfo ModuleInfo String
    | InvalidImportId ModuleInfo ImportId String
    | InvalidExportId ModuleInfo ExportId String
    | InvalidOperation String String
    | DuplicateModule ModuleInfo String
    | ParseError SrcLoc String String
    | Unsupported String
    | UserError u
    deriving Eq

instance Show u => Show (ProjectError u) where
    show (ModuleNotFound i s)
        = printf "%s: module \"%s\" not found" s (show i)
    show (DeclarationNotFound mi di s)
        = printf "%s: in module \"%s\" declaration \"%s\" not found" s (show mi) (show di)
    show (SymbolNotFound mi sym s)
        = printf "%s: in module \"%s\" symbol \"%s\" not found" s (show mi) (show sym)
    show (SymbolNotImported mi sym s)
        = printf "%s: module \"%s\" does not import symbol \"%s\"" s (show mi) (show sym)
    show (SymbolNotExported mi sym s)
        = printf "%s: module \"%s\" does not export symbol \"%s\"" s (show mi) (show sym)
    show (NotSubSymbol super sub s)
        = printf "%s: \"%s\" is not a class method or constructor of \"%s\" %s" s (show sub) (show super)
    show (ModuleNotImported importer importee s)
        = printf "%s: module \"%s\" does not import module \"%s\"" s (show importer) (show importee)
    show (InvalidImportId mi ii s)
        = printf "%s: module \"%s\" does not have an import with ID \"%s\"" s (show mi) (show ii)
    show (InvalidExportId mi ei s)
        = printf "%s: module \"%s\" does not have an export with ID \"%s\"" s (show mi) (show ei)
    show (InvalidOperation s1 s2)
        = printf "%s: %s" s2 s1
    show (DuplicateModule mi s)
        = printf "%s: a module named \"%s\" already exists" s (show mi)
    show (ParseError l msg s)
        = printf "%s: %s: %s" s (show l) msg
    show (Unsupported s)
        = s
    show (UserError u)
        = show u


class Qualify a where
    qual :: ModuleChild a -> Symbol
 
instance Qualify Symbol where
    qual (ModuleChild (ModuleInfo (Symbol m)) (Symbol s)) = Symbol $ m ++ '.' : s
    qual (ModuleChild (UnamedModule _) _) = error "Cannot qualify with an unnamed module"

instance Qualify DeclarationInfo where
    qual (ModuleChild (ModuleInfo (Symbol m)) (DeclarationInfo (Symbol s)))
        = Symbol $ m ++ '.' : s
    qual (ModuleChild (UnamedModule _) _) = error "Cannot qualifiy with an unnamed module"


type ProjectResult m u = ExceptT (ProjectError u) m
