{-|
Module      : Ide3.Types
Description : Top level types used by Ide3
Copyright   : (c) Andrew Melnick, 2016

License     : BSD3
Maintainer  : meln5674@kettering.edu
Stability   : experimental
Portability : POSIX

-}

module Ide3.Types 
    ( WithBody (..) 
    , body 
    , bodies 
    , item 
    , items 
    , Symbol (..) 
    , SolutionInfo (..) 
    , ProjectInfo (..) 
    , BuildInfo (..) 
    , Dependency (..) 
    , ModuleInfo (..) 
    , ModuleItem (..) 
    , ModuleItemKey (..) 
    , ModuleItemKeyValue (..) 
    , ModuleItemString (..) 
    , Pragma 
    , ModuleChild (..) 
    , ProjectChild (..) 
    , HasChild (..) 
    , Import (..) 
    , ImportId 
    , ImportKind (..) 
    , Export (..) 
    , ExternExport (..) 
    , ExportId 
    , DeclarationInfo (..) 
    , Declaration (..) 
    , TypeDeclaration (..) 
    , BindDeclaration (..) 
    , ModifierDeclaration (..) 
    , Constructor (..) 
    , SolutionError (..) 
    , SolutionResult 
    , Qualify (..)
    , moduleInfoString
    )
    where

import Ide3.Types.Internal
