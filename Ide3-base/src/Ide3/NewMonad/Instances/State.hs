module Ide3.NewMonad.Instances.State where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.State
import Control.Monad.Trans.Except

import Ide3.NewMonad
import Ide3.NewMonad.Instances.State.Class

import Ide3.Env
import qualified Ide3.Env.Solution as Solution

import Ide3.Types


instance StatefulSolutionClass m => SolutionClass (StatefulWrapper m) where
    editSolutionInfo f = modifySolution $ \s -> s{ solutionInfo = f $ solutionInfo s }
    addProject a = modifySolutionER $ \s -> runDescent2 Solution.addProject s a
    removeProject a = modifySolutionER $ \s -> runDescent2 Solution.removeProject s a
    getProjects = modifySolutionER $ \s -> runDescent1 Solution.getProjects s
    editProjectInfo a b = modifySolutionER $ \s -> runDescent3 Solution.editProjectInfo s a b

instance StatefulSolutionClass m => ProjectModuleClass (StatefulWrapper m) where
    addModule a b = modifySolutionER $ \s -> runDescent3 Solution.addModule s a b
    createModule a b = modifySolutionER $ \s -> runDescent3 Solution.createModule s a b
    getModule a b = modifySolutionER $ \s -> runDescent3 Solution.getModule s a b
    getModules a = modifySolutionER $ \s -> runDescent2 Solution.allModules s a
    editModule a b c = modifySolutionER $ \s -> runDescent4 Solution.editModule s a b c
    removeModule a b = modifySolutionER $ \s -> runDescent3 Solution.removeModule s a b

instance StatefulSolutionClass m => ProjectExternModuleClass (StatefulWrapper m) where
    addExternModule a b = modifySolutionER $ \s -> runDescent3 Solution.addExternModule s a b
    getExternModule a b = modifySolutionER $ \s -> runDescent3 Solution.getExternModule s a b
    getExternModules a = modifySolutionER $ \s -> runDescent2 Solution.getExternModules s a
    removeExternModule a b = modifySolutionER $ \s -> runDescent3 Solution.removeExternModule s a b

instance StatefulSolutionClass m => ModuleDeclarationClass (StatefulWrapper m) where
    addDeclaration a b c = modifySolutionER $ \s -> runDescent4 Solution.addDeclaration s a b c
    getDeclaration a b c  = modifySolutionER $ \s -> runDescent4 Solution.getDeclaration s a b c
    getDeclarations a b = modifySolutionER $ \s -> runDescent3 Solution.getDeclarations s a b
    editDeclaration a b c d = modifySolutionER $ \s -> runDescent5 Solution.editDeclaration s a b c d
    removeDeclaration a b c = modifySolutionER $ \s -> runDescent4 Solution.removeDeclaration s a b c

instance StatefulSolutionClass m => ModuleImportClass (StatefulWrapper m) where
    addImport a b c = modifySolutionER $ \s -> runDescent4 Solution.addImport s a b c
    getImport a b c = modifySolutionER $ \s -> runDescent4 Solution.getImport s a b c
    removeImport a b c = modifySolutionER $ \s -> runDescent4 Solution.removeImport s a b c
    getImports a b = modifySolutionER $ \s -> runDescent3 Solution.getImports s a b

instance StatefulSolutionClass m => ModuleExportClass (StatefulWrapper m) where
    addExport a b c = modifySolutionER $ \s -> runDescent4 Solution.addExport s a b c
    getExport a b c = modifySolutionER $ \s -> runDescent4 Solution.getExport s a b c
    removeExport a b c = modifySolutionER $ \s -> runDescent4 Solution.removeExport s a b c
    exportAll a b = modifySolutionER $ \s -> runDescent3 Solution.exportAll s a b
    exportNothing a b = modifySolutionER $ \s -> runDescent3 Solution.exportNothing s a b
    getExports a b = modifySolutionER $ \s -> runDescent3 Solution.getExports s a b

instance StatefulSolutionClass m => ModulePragmaClass (StatefulWrapper m) where
    addPragma a b c = modifySolutionER $ \s -> runDescent4 Solution.addPragma s a b c
    removePragma a b c = modifySolutionER $ \s -> runDescent4 Solution.removePragma s a b c
    getPragmas a b = modifySolutionER $ \s -> runDescent3 Solution.getPragmas s a b
