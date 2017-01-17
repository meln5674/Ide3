{-|
Module      : CabalFilesystemProject
Description : Persistance mechanism using .cabal files
Copyright   : (c) Andrew Melnick, 2016

License     : BSD3
Maintainer  : meln5674@kettering.edu
Stability   : experimental
Portability : POSIX

The Cabal persistance mechanism uses .cabal files to store the list of modules
in a solution, and stores declarations in the module files as normal
-}
module CabalFilesystemSolution
    ( CabalSolution(CabalSolution)
    , FileSystemSolution (Unopened)
    , runCabalSolution
    ) where

import CabalFilesystemSolution.Internal
