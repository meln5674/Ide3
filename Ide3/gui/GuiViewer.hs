{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module GuiViewer
    ( module GuiViewer.Internal
    ) where

import Control.Monad.Trans

import Ide3.Utils
import Ide3.NewMonad
import Ide3.NewMonad.Instances.Lift.TH

import Viewer

import GuiViewer.Internal

[betterderiving| lift; ; ViewerMonad; GuiViewerT; prepareBuild=splice2 |]

[betterderiving| lift; ; ViewerStateClass; GuiViewerT; |]

[betterderiving|
splice2;
;
  PersistenceClass
, SolutionClass
, ProjectModuleClass
, ModuleDeclarationClass
, ModuleImportClass
, ModuleExportClass
, ModulePragmaClass
, ModuleLocationClass
, ModuleFileClass
, ProjectExternModuleClass
, ExternModuleExportClass;
GuiViewerT;
|]

