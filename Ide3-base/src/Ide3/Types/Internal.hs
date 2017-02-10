{-|
Module      : Ide3.Types.Internal
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
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Ide3.Types.Internal where

import Data.Monoid

import Data.Text (Text)
import qualified Data.Text as T

import Control.Monad.Trans.Except

import Text.Printf

import Ide3.SrcLoc.Types

-- |Attaches a string ("body") to another type
data WithBody a = WithBody a Text
    deriving (Show, Read, Eq)

-- |Get the body attached to a value
body :: WithBody a -> Text
body (WithBody _ s) = s

-- |Get the bodies from a list of values
bodies :: [WithBody a] -> [Text]
bodies = map body

-- |Get the item a body is attached to
item :: WithBody a -> a
item (WithBody x _) = x

-- |Get the items from a list of values with bodies attached
items :: [WithBody a] -> [a]
items = map item

-- | Applies the function to the item while leaving the body unchanged
instance Functor WithBody where
    fmap f (WithBody x s) = WithBody (f x) s
    
-- | Catch-all type for any identifier which is significant to the program
newtype Symbol = Symbol { getSymbol :: Text }
    deriving (Show, Read, Eq, Ord)

-- |Join two symbols together such that the second is qualified by the first
joinSym :: Symbol -> Symbol -> Symbol
joinSym (Symbol x) (Symbol y) = Symbol $ x <> "." <> y


-- | Information on a solution
data SolutionInfo = SolutionInfo Text
  deriving (Show, Read, Eq, Ord)


-- |Information about a project
data ProjectInfo = ProjectInfo { unProjectInfo :: Text }
    deriving (Show, Read, Eq, Ord)
-- |Information on how to build a project
data BuildInfo = BuildInfo
    deriving (Show, Read, Eq)


-- | A dependency for a project
data Dependency = Dependency Text

-- | Information identifying a module
data ModuleInfo 
    -- |A module with a name
    = ModuleInfo Symbol              
    -- |An unamed module, possibly with the path it came from    
    | UnamedModule (Maybe FilePath)     
    deriving (Show, Read, Eq, Ord)

-- | Produce a string representing a module's info, with a default string if it
-- is an unnamed, pathless module
moduleInfoString :: ModuleInfo -> Text -> Text
moduleInfoString (ModuleInfo s) _ = getSymbol s
moduleInfoString (UnamedModule (Just path)) _ = T.pack path
moduleInfoString _ x = x

-- | A key-value pair of a module item
data ModuleItemKeyValue
    = HeaderCommentKeyValue Text
    | PragmaKeyValue Pragma
    | ImportKeyValue ImportId (WithBody Import)
    | ExportKeyValue ExportId (WithBody Export)
    | DeclarationKeyValue DeclarationInfo (WithBody Declaration)

-- | A key of a module item
data ModuleItemKey
    = HeaderCommentKey
    | PragmaKey Pragma
    | ImportKey ImportId
    | ExportKey ExportId
    | DeclarationKey DeclarationInfo

-- | A value of a module item
data ModuleItem
    = HeaderCommentItem Text
    | PragmaItem Pragma
    | ImportItem (WithBody Import)
    | ExportItem (WithBody Export)
    | DeclarationItem (WithBody Declaration)

-- | Information necessary to display a module item as a string
data ModuleItemString
    = HeaderCommentString Text
    | PragmaString Pragma
    | ImportString (WithBody ImportId)
    | ExportString (WithBody ExportId)
    | DeclarationString (WithBody DeclarationInfo)
  deriving Show

-- | A module pragma
type Pragma = Text

-- | An external export from an external module
data ExternExport
    = SingleExternExport Symbol
    | MultiExternExport Symbol [Symbol]
    deriving (Show, Eq, Read, Ord)

-- |A value which is tagged as belonging to a module
data ModuleChild a = ModuleChild ModuleInfo a
    deriving (Show, Eq, Ord)

-- | A value which is tagged as belonging to a project
data ProjectChild a = ProjectChild ProjectInfo a

-- | Convert the child to string, prepend the project name in parens
instance Show a => Show (ProjectChild a) where
    show (ProjectChild (ProjectInfo n) x) = show x ++ " ( " ++ T.unpack n ++ " )"

-- | Class of types which have a child
class HasChild f where
    -- | Retrieve the child value
    getChild :: f a -> a

-- | Retrieve child
instance HasChild ModuleChild where
    getChild (ModuleChild _ a) = a

-- | Retrieve child
instance HasChild ProjectChild where
    getChild (ProjectChild _ a) = a

-- | Apply function to child
instance Functor ModuleChild where
    fmap f (ModuleChild mi x) = ModuleChild mi $ f x

-- | Apply function to child
instance Foldable ModuleChild where
    foldMap f (ModuleChild _ x) = f x

-- | Apply function to child
instance Traversable ModuleChild where
    sequenceA (ModuleChild mi x) = ModuleChild mi <$> x

-- | Apply function to child
instance Functor ProjectChild where
    fmap f (ProjectChild pji x) = ProjectChild pji $ f x

-- | Apply function to child
instance Foldable ProjectChild where
    foldMap f (ProjectChild _ x) = f x

-- | Apply function to child
instance Traversable ProjectChild where
    sequenceA (ProjectChild pji x) = ProjectChild pji <$> x


-- |An import statement. The first three fields of each are:
--  The module being imported
--  Is it qualified?
--  The renamed symbol, if any
data Import
    -- |Importing a module normally
    = ModuleImport Symbol Bool (Maybe Symbol)
    -- |Importing only speci=fic parts of a module
    | WhitelistImport Symbol Bool (Maybe Symbol) [ImportKind]
    -- |Importing everthing but specific parts of a module
    | BlacklistImport Symbol Bool (Maybe Symbol) [ImportKind]
    deriving (Show, Read, Eq)

-- |Identifier for an import statement
newtype ImportId = ImportId { unImportId :: Int }
  deriving (Eq, Ord, Num, Enum)

instance Show ImportId where
    show = show . unImportId

instance Read ImportId where
    readsPrec = fmap (fmap (fmap (\(a,b) -> (ImportId a,b)))) readsPrec

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
newtype ExportId = ExportId { unExportId :: Int }
  deriving (Eq, Ord, Num, Enum)

instance Show ExportId where
    show = show . unExportId

instance Read ExportId where
    readsPrec = fmap (fmap (fmap (\(a,b) -> (ExportId a,b)))) readsPrec


-- |Information identifying a declaration
data DeclarationInfo
    -- | Info for declarations revoling around a single symbol
    = SymbolDeclarationInfo
        { getSymbolDeclarationInfo :: Symbol }
    -- | Cop-out for everything else
    | RawDeclarationInfo
        { getRawDeclarationInfo :: Text }
    deriving (Show, Read, Eq, Ord)


-- |A declaration
data Declaration
    -- | A type declaration creates types
    = TypeDeclaration DeclarationInfo TypeDeclaration
    -- | A bind declaration binds names and patterns to expressions
    | BindDeclaration DeclarationInfo BindDeclaration
    -- | A modifier declaration provides some property of an existing declaration
    | ModifierDeclaration DeclarationInfo ModifierDeclaration
    -- | A template haskell quasi-quote splice
    | SpliceDeclaration DeclarationInfo SpliceDeclaration
    -- | A declaration which cannot be parsed
    | UnparseableDeclaration DeclarationInfo
    deriving (Show, Read, Eq)


-- | A declaration which provides types
data TypeDeclaration
    -- | Class declaration
    = ClassDeclaration Symbol [Declaration]
    -- | Type synonym
    | TypeSynonym Symbol Symbol
    -- | Open type family
    | OpenTypeFamilyDecl Symbol
    -- | Closed type family
    | ClosedTypeFamilyDecl Symbol [[Symbol]]
    -- | Data declaration
    | DataDeclaration Symbol [Constructor]
    -- | Newtype declaration
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
    = FixityDeclaration [Symbol] FixityType
    -- |A instance of a class
    | InstanceDeclaration Symbol [Symbol] [Declaration]
    -- |A type signature of a bind
    | TypeSignatureDeclaration Symbol Symbol
    -- | A standalone deriving declaration
    | DerivingDeclaration Symbol [Symbol]
    | TypeFamilyInstanceDeclaration Symbol [Symbol]
    | DepricatedDeclaration [Symbol]
    | WarningDeclaration [Symbol]
    | InlineDeclaration Symbol InlineType
    | SpecialiseDeclaration Symbol SpecialiseType
    | MinimalDeclaration [Symbol]
    | RoleDeclaration Symbol
    deriving (Show, Read, Eq)

data InlineType = Inline | InlineConlike
  deriving (Show, Read, Eq)

data SpecialiseType = Specialise | SpecialiseInline
  deriving (Show, Read, Eq)

data SpliceDeclaration
    = QuasiQuoteDeclaration Text Text
  deriving (Show, Read, Eq)
  
-- |The type of fixity of an operator or infix function
data FixityType 
    = FixityLeft (Maybe Int)
    | FixityRight (Maybe Int)
    | FixityNone (Maybe Int)
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

-- | Class of types which can yield a list of symbols
class HasNames a where
    findName :: a -> [Symbol]

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
    | ParseError SrcFileLoc String String
    | Unsupported String
    | InternalError String String
    | UserError u
    deriving Eq

-- | Return error message for each error type
instance Show u => Show (SolutionError u) where
    show (ModuleNotFound pji mi s)
        = printf "%s: module \"%s\" not found in project \"%s\"" 
          s 
          (show mi)
          (show pji)
    show (DeclarationNotFound mi di s)
        = printf "%s: in module \"%s\" declaration \"%s\" not found" 
          s 
          (show mi) 
          (show di)
    show (SymbolNotFound mi sym s)
        = printf "%s: in module \"%s\" symbol \"%s\" not found" 
          s 
          (show mi) 
          (show sym)
    show (SymbolNotImported mi sym s)
        = printf "%s: module \"%s\" does not import symbol \"%s\"" 
          s 
          (show mi) 
          (show sym)
    show (SymbolNotExported mi sym s)
        = printf "%s: module \"%s\" does not export symbol \"%s\"" 
          s 
          (show mi) 
          (show sym)
    show (NotSubSymbol super sub s)
        = printf "%s: \"%s\" is not a class method or constructor of \"%s\" %s" 
          s 
          (show sub) 
          (show super)
    show (ModuleNotImported pji importer importee s)
        = printf
          "%s: module \"%s\" does not import module \"%s\" in project \"%s\""
          s 
          (show importer) 
          (show importee) 
          (show pji)
    show (InvalidImportId mi ii s)
        = printf "%s: module \"%s\" does not have an import with ID \"%s\"" 
          s 
          (show mi) 
          (show ii)
    show (InvalidExportId mi ei s)
        = printf "%s: module \"%s\" does not have an export with ID \"%s\""
          s
          (show mi)
          (show ei)
    show (InvalidOperation s1 s2)
        = printf "%s: %s" s2 s1
    show (DuplicateDeclaration mi di s)
        = printf "%s: A declaration \"%s\" already exists in module \"%s\""
          s 
          (show di) 
          (show mi)
    show (DuplicateModule pji mi s)
        = printf "%s: a module named \"%s\" already exists in project \"%s\""
          s 
          (show mi)
          (show pji)
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

-- | Class of symbols which can be combined with a module info to produce a
-- symbol
class Qualify a where
    -- | Qualify a value with a module prefix
    qual :: ModuleChild a -> Symbol
 
-- | Prepend module name and dot to symbol name
instance Qualify Symbol where
    qual (ModuleChild (ModuleInfo (Symbol m)) (Symbol s))
        = Symbol $ m <> "." <> s
    qual (ModuleChild (UnamedModule _) _)
        = error "Cannot qualify with an unnamed module"

-- | Prepend module name and dot to declaration info
instance Qualify DeclarationInfo where
    qual (ModuleChild (ModuleInfo (Symbol m)) (SymbolDeclarationInfo (Symbol s)))
        = Symbol $ m <> "." <> s
    qual (ModuleChild (ModuleInfo _) (RawDeclarationInfo _))
        = error "Cannot qualify a non-symbol declaration"
    qual (ModuleChild (UnamedModule _) _)
        = error "Cannot qualifiy with an unnamed module"

-- | Wrapper for a monad transformer which can throw solution exceptions
type SolutionResult u = ExceptT (SolutionError u)
