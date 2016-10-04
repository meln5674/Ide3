module DeclarationPath 
    ( SolutionPath (..)
    , ItemPath
    , parse
    ) where

import Control.Monad

import Text.Parsec hiding (parse)
import Text.Parsec.Char

import Ide3.Types

type ItemPath = ProjectChild (ModuleChild (Maybe ModuleItemString))

data SolutionPath
    = DeclarationPath ProjectInfo ModuleInfo DeclarationInfo
    | ImportPath ProjectInfo ModuleInfo ImportId
    | ImportsPath ProjectInfo ModuleInfo
    | ExportPath ProjectInfo ModuleInfo ExportId
    | ExportsPath ProjectInfo ModuleInfo
    | PragmaPath ProjectInfo ModuleInfo Pragma
    | PragmasPath ProjectInfo ModuleInfo
    | ModulePath ProjectInfo ModuleInfo
    | ProjectPath ProjectInfo
    | SolutionPath
  deriving Eq


instance Show SolutionPath where
    show SolutionPath = "/"
    show (ProjectPath (ProjectInfo a)) = "/" ++ a ++ "/"
    show (ModulePath (ProjectInfo a) (ModuleInfo (Symbol b))) = "/" ++ a ++ "/" ++ b ++ ":"
    show (PragmasPath (ProjectInfo a) (ModuleInfo (Symbol b))) = "/" ++ a ++ "/" ++ b ++ ":#"
    show (PragmaPath (ProjectInfo a) (ModuleInfo (Symbol b)) p) = "/" ++ a ++ "/" ++ b ++ ":#" ++ p
    show (ExportsPath (ProjectInfo a) (ModuleInfo (Symbol b))) = "/" ++ a ++ "/" ++ b ++ ":<"
    show (ExportPath (ProjectInfo a) (ModuleInfo (Symbol b)) ei) = "/" ++ a ++ "/" ++ b ++ ":<" ++ show ei
    show (ImportsPath (ProjectInfo a) (ModuleInfo (Symbol b))) = "/" ++ a ++ "/" ++ b ++ ":>"
    show (ImportPath (ProjectInfo a) (ModuleInfo (Symbol b)) ii) = "/" ++ a ++ "/" ++ b ++ ":>" ++ show ii
    show (DeclarationPath (ProjectInfo a) (ModuleInfo (Symbol b)) (DeclarationInfo (Symbol c)))
        = a ++ "/" ++ b ++ ":" ++ c

projectName :: Parsec String () ProjectInfo
projectName = liftM ProjectInfo $ many $ notFollowedBy (char '/') *> anyToken

moduleName:: Parsec String () ModuleInfo
moduleName = liftM (ModuleInfo . Symbol) $ many $ notFollowedBy (char ':') *> anyToken

declarationInfo :: Parsec String () DeclarationInfo 
declarationInfo = liftM (DeclarationInfo . Symbol) $ many anyToken

declarationPath :: Parsec String () SolutionPath
declarationPath = DeclarationPath <$> projectName <*> (char '/' *> moduleName) <*> (char ':' *> declarationInfo)

modulePath :: Parsec String () SolutionPath
modulePath = ModulePath <$> projectName <*> (char '/' *> moduleName <* optional (char ':'))

projectPath :: Parsec String () SolutionPath
projectPath = ProjectPath <$> projectName <* optional (char '/')

anyPath :: Parsec String () SolutionPath
anyPath = choice $ map try [declarationPath, modulePath, projectPath]

parse :: String -> Maybe SolutionPath
parse s = case runParser anyPath () "" s of
    Right path -> Just path
    Left _ -> Nothing
