module DeclarationPath 
    ( DeclarationPath (..)
    , parse
    ) where

import Control.Monad

import Text.Parsec hiding (parse)
import Text.Parsec.Char

import Ide3.Types

data DeclarationPath 
    = DeclarationPath ProjectInfo ModuleInfo DeclarationInfo
    | ModulePath ProjectInfo ModuleInfo
    | ProjectPath ProjectInfo

instance Show DeclarationPath where
    show (ProjectPath (ProjectInfo a)) = a ++ "/"
    show (ModulePath (ProjectInfo a) (ModuleInfo (Symbol b))) = a ++ "/" ++ b ++ ":"
    show (DeclarationPath (ProjectInfo a) (ModuleInfo (Symbol b)) (DeclarationInfo (Symbol c)))
        = a ++ "/" ++ b ++ ":" ++ c

projectName :: Parsec String () ProjectInfo
projectName = liftM ProjectInfo $ many $ notFollowedBy (char '/') *> anyToken

moduleName:: Parsec String () ModuleInfo
moduleName = liftM (ModuleInfo . Symbol) $ many $ notFollowedBy (char ':') *> anyToken

declarationInfo :: Parsec String () DeclarationInfo 
declarationInfo = liftM (DeclarationInfo . Symbol) $ many anyToken

declarationPath :: Parsec String () DeclarationPath
declarationPath = DeclarationPath <$> projectName <*> (char '/' *> moduleName) <*> (char ':' *> declarationInfo)

modulePath :: Parsec String () DeclarationPath
modulePath = ModulePath <$> projectName <*> (char '/' *> moduleName <* optional (char ':'))

projectPath :: Parsec String () DeclarationPath
projectPath = ProjectPath <$> projectName <* optional (char '/')

anyPath :: Parsec String () DeclarationPath
anyPath = choice $ map try [declarationPath, modulePath, projectPath]

parse :: String -> Maybe DeclarationPath
parse s = case runParser anyPath () "" s of
    Right path -> Just path
    Left _ -> Nothing
