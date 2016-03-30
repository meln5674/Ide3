module Ide3.Constructor where

import Ide3.Types

symbol :: Constructor -> Symbol
symbol (PrefixConstructor s _) = s
symbol (InfixConstructor _ s _) = s
symbol (RecordConstructor s _) = s

bindsProvided :: Constructor -> [Symbol]
bindsProvided (PrefixConstructor _ _) = []
bindsProvided (InfixConstructor _ _ _) = []
bindsProvided (RecordConstructor _ ss) = map fst ss

