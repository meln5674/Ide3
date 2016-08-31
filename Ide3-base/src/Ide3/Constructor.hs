{-|
Module      : Ide3.Constructor
Description : Operations on constructors
Copyright   : (c) Andrew Melnick, 2016

License     : BSD3
Maintainer  : meln5674@kettering.edu
Stability   : experimental
Portability : POSIX
-}
module Ide3.Constructor where

import Ide3.Types.Internal


-- | Get the symbol of a constructor
symbol :: Constructor -> Symbol
symbol (PrefixConstructor s _) = s
symbol (InfixConstructor _ s _) = s
symbol (RecordConstructor s _) = s

-- | Get the binds provided by a constructor
bindsProvided :: Constructor -> [Symbol]
bindsProvided PrefixConstructor{} = []
bindsProvided InfixConstructor{} = []
bindsProvided (RecordConstructor _ ss) = map fst ss

