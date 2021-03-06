{-|
Module      : DeclarationPath
Description : Textual representation of locations of module elements
Copyright   : (c) Andrew Melnick, 2016

License     : BSD3
Maintainer  : meln5674@kettering.edu
Stability   : experimental
Portability : POSIX

The SolutionPath constructors contain all information neccessary to look up a
solution element.
-}

{-# LANGUAGE OverloadedStrings #-}
module DeclarationPath 
    ( SolutionPath (..)
    , ItemPath
    , parse
    ) where

import Data.Monoid

import Data.Text (Text)
import qualified Data.Text as T

import Control.Monad

import Text.Parsec hiding (parse)

import Ide3.Types

-- | Lookup information for a module item, potentially missing the item itself
type ItemPath = ProjectChild (ModuleChild (Maybe ModuleItemString))

-- | Lookup information for any item in a solution
data SolutionPath
    = 
    -- | Lookup information for a declaration
      DeclarationPath ProjectInfo ModuleInfo DeclarationInfo
    -- | Lookup information for an import
    | ImportPath ProjectInfo ModuleInfo ImportId
    -- | Lookup information for the import collection
    | ImportsPath ProjectInfo ModuleInfo
    -- | Lookup information for an export
    | ExportPath ProjectInfo ModuleInfo ExportId
    -- | Lookup information for the export collection
    | ExportsPath ProjectInfo ModuleInfo
    -- | Lookup information for a pragma
    | PragmaPath ProjectInfo ModuleInfo Pragma
    -- | Lookup information for the pragma collection
    | PragmasPath ProjectInfo ModuleInfo
    -- | Lookup information for a module
    | ModulePath ProjectInfo ModuleInfo
    -- | Lookup information for an unparsable module
    | UnparsableModulePath ProjectInfo ModuleInfo
    -- | Lookup information for a project
    | ProjectPath ProjectInfo
    -- | Lookup information for a solution
    | SolutionPath
  deriving (Eq, Ord)


class ToSolutionPath a where
    toSolutionPath :: a -> Text


instance ToSolutionPath ProjectInfo where
    toSolutionPath (ProjectInfo x) = x <> "/"
instance ToSolutionPath ModuleInfo where
    toSolutionPath (ModuleInfo (Symbol x)) = x <> ":"
instance ToSolutionPath DeclarationInfo where
    toSolutionPath (SymbolDeclarationInfo (Symbol x)) = x
    toSolutionPath (RawDeclarationInfo x) = x
instance ToSolutionPath ImportId where
    toSolutionPath ii = "[IMPORT ID=" <> T.pack (show ii) <> "]"
instance ToSolutionPath ExportId where
    toSolutionPath ei = "[EXPORT ID=" <> T.pack (show ei) <> "]"

instance Show SolutionPath where
    show SolutionPath 
        = ""
    show (ProjectPath pji) 
        = T.unpack $ toSolutionPath pji

    show (ModulePath pji mi)
        = T.unpack $ toSolutionPath pji <> toSolutionPath mi

    show (UnparsableModulePath pji mi)
        = T.unpack $ toSolutionPath pji <> toSolutionPath mi

    show (PragmasPath pji mi)
        = T.unpack $ toSolutionPath pji <> toSolutionPath mi <> "[PRAGMAS]"
        
    show (PragmaPath pji mi p) 
        = T.unpack $ toSolutionPath pji <> toSolutionPath mi <> "[PRAGMA " <> p <> "]"

    show (ExportsPath pji mi) 
        = T.unpack $ toSolutionPath pji <> toSolutionPath mi <> "[EXPORTS]"

    show (ExportPath pji mi ei) 
        = T.unpack $ toSolutionPath pji <> toSolutionPath mi <> toSolutionPath ei

    show (ImportsPath pji mi)
        = T.unpack $ toSolutionPath pji <> toSolutionPath mi <> "[IMPORTS]"

    show (ImportPath pji mi ii) 
        = T.unpack $ toSolutionPath pji <> toSolutionPath mi <> toSolutionPath ii

    show (DeclarationPath pji mi di)
        = T.unpack $ toSolutionPath pji <> toSolutionPath mi <> toSolutionPath di
    


-- | Parser for the terminator between project and module names
projectTerminator :: Parsec String () ()
projectTerminator = void $ char '/'

-- | Parser for the terminator between module and module items
moduleTerminator :: Parsec String () ()
moduleTerminator = void $ char ':'

-- | Parser for a project name
projectName :: Parsec String () ProjectInfo
projectName = (ProjectInfo . T.pack) <$> many (notFollowedBy projectTerminator *> anyToken)

-- | Parser for a module name
moduleName:: Parsec String () ModuleInfo
moduleName = (ModuleInfo . Symbol . T.pack) <$> many (notFollowedBy moduleTerminator  *> anyToken)

-- | Parser for a declaration info
declarationInfo :: Parsec String () DeclarationInfo 
declarationInfo = (SymbolDeclarationInfo . Symbol . T.pack) <$> many anyToken

-- | Parser for a declaration path
declarationPath :: Parsec String () SolutionPath
declarationPath = DeclarationPath 
    <$> projectName 
    <*> (projectTerminator *> moduleName) 
    <*> (moduleTerminator *> declarationInfo)

-- | Parser for a module path
modulePath :: Parsec String () SolutionPath
modulePath = ModulePath 
    <$> projectName 
    <*> (projectTerminator *> moduleName <* optional moduleTerminator)

-- | Parser for a project path
projectPath :: Parsec String () SolutionPath
projectPath = ProjectPath <$> projectName <* optional projectTerminator

-- | Parser for a declaration, module, or project path
anyPath :: Parsec String () SolutionPath
anyPath = choice $ map try [declarationPath, modulePath, projectPath]

-- | Parse a declaration, module, or project path
parse :: String -> Maybe SolutionPath
parse s = case runParser anyPath () "" s of
    Right path -> Just path
    Left _ -> Nothing
