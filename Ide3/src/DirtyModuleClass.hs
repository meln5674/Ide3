module DirtyModuleClass where

import Ide3.Types

class DirtyModuleClass m where
    isModuleDirty :: ProjectInfo -> ModuleInfo -> SolutionResult u m Bool
