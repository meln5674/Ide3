{-|
Module      : Ide3.NewMonad.Instances.State.ModuleDeclarationClass
Description : Stateful implementation of the ModuleDeclarationClass
Copyright   : (c) Andrew Melnick, 2016

License     : BSD3
Maintainer  : meln5674@kettering.edu
Stability   : experimental
Portability : POSIX

-}

module Ide3.NewMonad.Instances.State.ModuleDeclarationClass where

import Control.Lens
import Control.Monad.Except

import Ide3.Utils
import Ide3.NewMonad
import Ide3.NewMonad.Instances.State.Class

import Ide3.Env
--import qualified Ide3.Env.Solution as Solution

import qualified Ide3.Declaration as Declaration

import qualified Ide3.Solution.Lens as Solution
import qualified Ide3.Project.Lens as Project
import qualified Ide3.Module.Lens as Module

import Ide3.Types (item)

-- | Access declarations statefully
instance StatefulSolutionClass m 
      => ModuleDeclarationClass (StatefulWrapper m) where
{-
    addDeclaration = modifySolutionER .-... runDescent4 Solution.addDeclaration
    getDeclaration = modifySolutionER .-... runDescent4 Solution.getDeclaration
    getDeclarations = modifySolutionER .-.. runDescent3 Solution.getDeclarations
    editDeclaration =
        modifySolutionER .-.... runDescent5 Solution.editDeclaration 
    removeDeclaration =
        modifySolutionER .-... runDescent4 Solution.removeDeclaration
-}
    addDeclaration pji mi d = checkThenDo
        (Solution.uncheckDeclaration (pji, mi, di))
        (Solution.overLocals . ix (pji, mi) . Module.asParsable . Module.overDeclarations . at di ?~ d)
      where
        di = Declaration.info $ item d
    getDeclaration pji mi di = checkThenGet
        (Solution.checkDeclaration (pji, mi, di))
        (Solution.overDeclarations . ix (pji, mi, di))
    getDeclarations pji mi = checkThenGet
        (Solution.checkLocalModule (pji, mi))
        (Solution.overLocals . ix (pji, mi) . Module.asParsable . Module.getDeclarations)
    editDeclaration pji mi di f = do
        d <- checkThenGet
            (Solution.checkDeclaration (pji, mi, di))
            (Solution.overDeclarations . ix (pji, mi, di))
        case f d of
            Left err -> throwError err
            Right d' -> do
                let di' = Declaration.info $ item d'
                when (di' /= di) $ do
                    modifySolution $ Solution.overLocals . ix (pji, mi) . Module.asParsable . Module.overDeclarations . at di .~ Nothing
                    modifySolution $ Solution.overLocals . ix (pji, mi) . Module.asParsable . Module.overDeclarations . at di' ?~ d'
                return di'
    removeDeclaration pji mi di = checkThenDo
        (Solution.checkDeclaration (pji, mi, di))
        (Solution.overLocals . ix (pji, mi) . Module.asParsable . Module.overDeclarations . at di .~ Nothing)
