module Ide3.Module.Common.Types where

import Ide3.Types

-- | Wrapper for either kind of module
type EitherModule = Either Module ExternModule

