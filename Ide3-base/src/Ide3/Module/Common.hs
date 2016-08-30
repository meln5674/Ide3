{-|
Module      : Ide3.Module.Common
Description : Operations common to local and external modules
Copyright   : (c) Andrew Melnick, 2016

License     : BSD3
Maintainer  : meln5674@kettering.edu
Stability   : experimental
Portability : POSIX
-}

module Ide3.Module.Common 
    ( module Ide3.Module.Common
    , EitherModule
    ) where

import Ide3.Types.Internal
import qualified Ide3.Module.Internal as Local (info)
import qualified Ide3.Module.Extern as Extern

import Ide3.Module.Common.Types

-- | Get the identifying information from either a local or external module
info :: EitherModule -> ModuleInfo
info (Right m) = Extern.info m
info (Left m) = Local.info m
