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
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Ide3.Types.Internal where

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

-- |
instance Functor WithBody where
    -- |Applies the function to the item while leaving the body unchanged
    fmap f (WithBody x s) = WithBody (f x) s
    
-- | Catch-all type for any identifier which is significant to the program
newtype Symbol = Symbol { getSymbol :: String }
    deriving (Show, Read, Eq, Ord)

-- |Join two symbols together such that the second is qualified by the first
joinSym :: Symbol -> Symbol -> Symbol
joinSym (Symbol x) (Symbol y) = Symbol $ x ++ "." ++ y


-- | Information on a solution
data SolutionInfo = SolutionInfo String
  deriving (Show, Read, Eq, Ord)

-- | A solution, a collection of projects
data Solution
    = Solution
    { solutionInfo :: SolutionInfo
    , solutionProjects :: Map ProjectInfo Project
    }
  deriving (Show, Read)

-- |Information about a project
data ProjectInfo = ProjectInfo String
    deriving (Show, Read, Eq, Ord)
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

-- | A dependency for a project
data Dependency = Dependency String

-- | Information identifying a module
data ModuleInfo 
    -- |A module with a name
    = ModuleInfo Symbol              
    -- |An unamed module, possibly with the path it came from    
    | UnamedModule (Maybe FilePath)     
    deriving (Show, Read, Eq, Ord)

-- | Get the name of a module from its info
getModuleName :: ModuleInfo -> Symbol
getModuleName (ModuleInfo n) = n
getModuleName _ = Symbol "UNNAMED MODULE"

-- | A module pragma
type Pragma = String

-- | A collection of imports
type ImportCollection = Map ImportId (WithBody Import)

-- | A collection of exports, or a mark that everything is exported
type ExportCollection = Maybe (Map ExportId (WithBody Export))

-- | A collection of declarations
type DeclarationCollection = Map DeclarationInfo (WithBody Declaration)

-- | A module. 
data Module
    = Module 
    { moduleInfo :: ModuleInfo  -- ^ Identifying information
    , moduleHeader :: String -- ^ Header text
    , modulePragmas :: [Pragma] -- ^ Pragmas
    , moduleImports :: ImportCollection
    , moduleExports :: ExportCollection
    , moduleDeclarations :: DeclarationCollection
    }
    deriving (Show, Read, Eq)

-- | An external export from an external module
data ExternExport
    = SingleExternExport Symbol
    | MultiExternExport Symbol [Symbol]
    deriving (Show, Eq, Read, Ord)

-- | A module which is external to the project, only a list of exported symbols
--  are availible
data ExternModule
    = ExternModule
    { externModuleInfo :: ModuleInfo
    , externModuleExports :: [ExternExport]
    }
    deriving (Show, Eq, Read, Ord)

-- |A value which is tagged as belonging to a module
data ModuleChild a = ModuleChild ModuleInfo a
    deriving (Show, Eq, Ord)

data ProjectChild a = ProjectChild ProjectInfo a

instance Show a => Show (ProjectChild a) where
    show (ProjectChild (ProjectInfo n) x) = show x ++ " ( " ++ n ++ " )"

class HasChild f where
    getChild :: f a -> a

instance HasChild ModuleChild where
    getChild (ModuleChild _ a) = a

instance HasChild ProjectChild where
    getChild (ProjectChild _ a) = a

-- |
instance Functor ModuleChild where
    fmap f (ModuleChild mi x) = ModuleChild mi $ f x

instance Functor ProjectChild where
    fmap f (ProjectChild pji x) = ProjectChild pji $ f x

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
    | UnparseableDeclaration DeclarationInfo
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
    -- | A standalone deriving declaration
    | DerivingDeclaration Symbol [Symbol]
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

-- | 
instance ToSym (Name a) where
    toSym (Ident _ n)         = Symbol n
    toSym (Syntax.Symbol _ n) = Symbol n

-- | 
instance ToSym (CName a) where
    toSym (VarName _ n) = toSym n
    toSym (ConName _ n) = toSym n

-- | 
instance ToSym (ModuleName a) where
    toSym (ModuleName _ n) = Symbol n

-- | 
instance ToSym (SpecialCon a) where
    toSym (UnitCon _)   = Symbol "()"
    toSym (ListCon _)   = Symbol "[]"
    toSym (FunCon _)    = Symbol "->"
    toSym (TupleCon _ Unboxed n) = Symbol $ "(" ++ replicate n ',' ++ ")"
    toSym (TupleCon _ Boxed n) = Symbol $ "(#" ++ replicate n ',' ++ "#)"
    toSym (Cons _) = Symbol ":"
    toSym (UnboxedSingleCon _) = Symbol "(# #)"

-- | 
instance ToSym (QName a) where
    toSym (Qual _ m n) = toSym m `joinSym` toSym n
    toSym (UnQual _ n) = toSym n
    toSym (Special _ s) = toSym s

-- | 
instance SrcInfo a => ToSym (Syntax.Type a) where
    toSym = Symbol . prettyPrint

-- | 
instance ToSym (DeclHead a) where
    toSym (DHead _ n) = toSym n
    toSym (DHInfix _ _ n) = toSym n
    toSym (DHParen _ h) = toSym h
    toSym (DHApp _ h _) = toSym h

-- |Errors that can arrise during modifying and querying a project
data SolutionError u
    = ModuleNotFound ProjectInfo ModuleInfo String
    | DeclarationNotFound ModuleInfo DeclarationInfo String
    | SymbolNotFound ModuleInfo Symbol String
    | SymbolNotImported ModuleInfo Symbol String
    | SymbolNotExported ModuleInfo Symbol String
    | NotSubSymbol Symbol Symbol String
    | ModuleNotImported ProjectInfo ModuleInfo ModuleInfo String
    | InvalidImportId ModuleInfo ImportId String
    | InvalidExportId ModuleInfo ExportId String
    | InvalidOperation String String
    | DuplicateDeclaration ModuleInfo DeclarationInfo String
    | DuplicateModule ProjectInfo ModuleInfo String
    | DuplicateProject ProjectInfo String
    | ProjectNotFound ProjectInfo String
    | ParseError SrcLoc String String
    | Unsupported String
    | InternalError String String
    | UserError u
    deriving Eq

-- | 
instance Show u => Show (SolutionError u) where
    show (ModuleNotFound pji mi s)
        = printf "%s: module \"%s\" not found in project \"%s\"" s (show mi) (show pji)
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
    show (ModuleNotImported pji importer importee s)
        = printf "%s: module \"%s\" does not import module \"%s\" in project \"%s\"" s (show importer) (show importee) (show pji)
    show (InvalidImportId mi ii s)
        = printf "%s: module \"%s\" does not have an import with ID \"%s\"" s (show mi) (show ii)
    show (InvalidExportId mi ei s)
        = printf "%s: module \"%s\" does not have an export with ID \"%s\"" s (show mi) (show ei)
    show (InvalidOperation s1 s2)
        = printf "%s: %s" s2 s1
    show (DuplicateDeclaration mi di s)
        = printf "%s: A declaration \"%s\" already exists in module \"%s\"" s (show di) (show mi)
    show (DuplicateModule pji mi s)
        = printf "%s: a module named \"%s\" already exists in project \"%s\"" s (show mi) (show pji)
    show (DuplicateProject pji s)
        = printf "%s: a project name \"%s\" already exists" s (show pji)
    show (ProjectNotFound pji s)
        = printf "%s: No project named \"%s\" exists" s (show pji)
    show (ParseError l msg s)
        = printf "Parse error %s: %s: %s" s (show l) msg
    show (Unsupported s)
        = printf "Unsupported: %s" s
    show (InternalError msg s)
        = printf "An internal error occured: %s: %s" s msg
    show (UserError u)
        = show u

-- | Class of symbols which can be combined with a module info to produce a symbol
class Qualify a where
    -- | Qualify a value with a module prefix
    qual :: ModuleChild a -> Symbol
 
-- | 
instance Qualify Symbol where
    qual (ModuleChild (ModuleInfo (Symbol m)) (Symbol s)) = Symbol $ m ++ '.' : s
    qual (ModuleChild (UnamedModule _) _) = error "Cannot qualify with an unnamed module"

-- | 
instance Qualify DeclarationInfo where
    qual (ModuleChild (ModuleInfo (Symbol m)) (DeclarationInfo (Symbol s)))
        = Symbol $ m ++ '.' : s
    qual (ModuleChild (UnamedModule _) _) = error "Cannot qualifiy with an unnamed module"

-- | Wrapper for a monad transformer which can throw solution exceptions
type SolutionResult m u = ExceptT (SolutionError u) m

{-
data ProjectParam a = ProjectParam ProjectInfo a

data ModuleParam a = ModuleParam ProjectInfo ModuleInfo a

data DeclarationParam a = DeclarationParam ProjectInfo ModuleInfo DeclarationInfo a

instance Functor ProjectParam where
    fmap f (ProjectParam a x) = ProjectParam a $ f x

instance Functor ModuleParam where
    fmap f (ModuleParam a b x) = ModuleParam a b $ f x

instance Functor DeclarationParam where
    fmap f (DeclarationParam a b c x) = DeclarationParam a b c $ f x

class ParamClass f a where
    getParam :: f a -> a
    setParam :: f b -> a -> f a

class ProjectParamClass f where
    getProjectInfo :: f a -> ProjectInfo
    
class ModuleParamClass f where
    getModuleInfo :: f a -> ModuleInfo

class DeclarationParamClass f where
    getDeclarationInfo :: f a -> DeclarationInfo

class UnwrapProject f where
    unwrapProject :: f a -> ProjectParam ModuleInfo

class UnwrapModule f where
    unwrapModule :: f a -> ModuleParam DeclarationInfo

instance UnwrapProject ModuleParam where
    unwrapProject (ModuleParam a b _) = ProjectParam a b

instance UnwrapProject DeclarationParam where
    unwrapProject (DeclarationParam a b _ _) = ProjectParam a b

instance UnwrapModule DeclarationParam where
    unwrapModule (DeclarationParam a b c _) = ModuleParam a b c

wrapProject :: ProjectParam ModuleInfo -> a -> ModuleParam a
wrapProject (ProjectParam a b) c = ModuleParam a b c

wrapModule :: ModuleParam DeclarationInfo -> a -> DeclarationParam a
wrapModule (ModuleParam a b c) d = DeclarationParam a b c d

instance ParamClass ProjectParam a where
    getParam (ProjectParam _ x) = x
    setParam (ProjectParam a _) x = ProjectParam a x

instance ParamClass ModuleParam a where
    getParam (ModuleParam _ _ x) = x
    setParam (ModuleParam a b _) x = ModuleParam a b x

instance ParamClass DeclarationParam a where
    getParam (DeclarationParam _ _ _ x) = x
    setParam (DeclarationParam a b c _) x = DeclarationParam a b c x
    
instance ProjectParamClass ProjectParam where
    getProjectInfo (ProjectParam x _) = x

instance ProjectParamClass ModuleParam where
    getProjectInfo (ModuleParam x _ _) = x

instance ProjectParamClass DeclarationParam where
    getProjectInfo (DeclarationParam x _ _ _) = x

instance ModuleParamClass ModuleParam where
    getModuleInfo (ModuleParam _ x _) = x

instance ModuleParamClass DeclarationParam where
    getModuleInfo (DeclarationParam _ x _ _) = x

instance DeclarationParamClass DeclarationParam where
    getDeclarationInfo (DeclarationParam _ _ x _) = x
-}
