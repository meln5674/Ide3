{-|
Module      : Ide3.Module.Extern
Description : External modules
Copyright   : (c) Andrew Melnick, 2016

License     : BSD3
Maintainer  : meln5674@kettering.edu
Stability   : experimental
Portability : POSIX
-}
module Ide3.Module.Extern where

import Ide3.Types.Internal

-- | Get the identifying information from an external module
info :: ExternModule -> ModuleInfo
info (ExternModule i _) = i

-- | Get the symbols exported by an external module
exportSymbols :: ExternExport -> [Symbol]
exportSymbols (SingleExternExport s) = [s]
exportSymbols (MultiExternExport s ss) = s : ss

-- | Get the symbols exported by an external module, annotated with that modules
-- identifying information
exportedSymbols :: ExternModule -> [ModuleChild Symbol]
exportedSymbols (ExternModule i es) = do
    syms <- map exportSymbols es
    map (ModuleChild i) syms
