module Ide3.Module 
    ( module Ide3.Module
    , info
    , new
    , empty
    ) where

import Ide3.Module.Internal

import Ide3.Types.Internal

infoMatches :: Module -> ModuleInfo -> Bool
