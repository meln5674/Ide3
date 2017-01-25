module GuiClass.Types 
    ( module GuiClass.Types
    , Row (..)
    , Column (..)
    ) where

import Ide3.Types

{-
newtype Row = Row { getRow :: Int } deriving (Eq, Ord)
newtype Column = Column { getColumn :: Int } deriving (Eq, Ord)
-}

type CursorPosition = (Row,Column)
type TreePath = [Int]

data SolutionTreeElem
    = ProjectElem ProjectInfo
    | ModuleElem ModuleInfo Bool
    | UnparsableModuleElem ModuleInfo SrcLoc String
    | DeclElem DeclarationInfo
    | ImportsElem
    | ExportsElem
    | PragmasElem
    | ImportElem ImportId (WithBody Import)
    | ExportElem ExportId (WithBody Export)
    | PragmaElem Pragma
    | SolutionElem
  deriving (Eq, Show)
