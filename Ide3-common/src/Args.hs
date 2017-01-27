module Args where

import Data.String

-- | Class for types which can be parsed from a list of strings
class Args a where
    getArgsFrom :: [String] -> Either String a

