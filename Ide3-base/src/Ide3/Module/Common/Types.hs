{-|
Module      : Ide3.Module.Common.Types
Description : 
Copyright   : (c) Andrew Melnick, 2016

License     : BSD3
Maintainer  : meln5674@kettering.edu
Stability   : experimental
Portability : POSIX
-}
module Ide3.Module.Common.Types where

import Ide3.Types.State

-- | Wrapper for either kind of module
type EitherModule = Either Module ExternModule

