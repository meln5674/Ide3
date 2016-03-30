module Ide3.Declaration.ModifierDeclaration where

import Ide3.Types

symbolsAffected :: ModifierDeclaration -> [Symbol]
symbolsAffected (FixityDeclaration ss _ _) = ss
symbolsAffected (InstanceDeclaration _ ss _) = ss
