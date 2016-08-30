module Initializer.Stack where

import Initializer

import System.Exit
import System.Process
import System.Directory
import System.FilePath

import Data.List

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Except

import Ide3.Types
import Ide3.NewMonad

import Args

-- | The name of a stack template
type TemplateName = String

-- | The arguments to initialize a stack solution, a file path and optional a template name
data StackInitializerArgs = StackInitializerArgs FilePath (Maybe TemplateName)

instance Args StackInitializerArgs where
    getArgsFrom [path] = Right $ StackInitializerArgs path Nothing
    getArgsFrom [template,path] = Right $ StackInitializerArgs path (Just template)
    getArgsFrom [] = Left "new expects at least a path and optionally a template name"
    getArgsFrom _ = Left "new expects only a path and optionally a template name"




-- | An Initializer that uses the stack new command to create a new solution
stackInitializer :: ( MonadIO m
                    {-, SolutionClass m
                    , ProjectModuleClass m
                    , ProjectExternModuleClass m
                    , PersistenceClass m-}
                    ) 
                 => Initializer StackInitializerArgs m
stackInitializer = Initializer $ \(StackInitializerArgs path template) -> do
    let args = case template of
            Nothing -> ["new", path]
            Just template_name -> ["new", path, template_name]
    (ec, out, err) <- liftIO $ readProcessWithExitCode "stack" args ""
    case ec of
        ExitSuccess -> do
            {-
            files <- liftM (delete "." . delete "..") $ liftIO $ getDirectoryContents path
            liftIO $ setCurrentDirectory path
            dirs <- liftIO $ filterM doesDirectoryExist files
            let solutionName = takeBaseName path
                solutionInfo = SolutionInfo solutionName
                projects = flip map dirs $ \d -> (ProjectInfo $ takeBaseName d, d,Nothing)
            digestSolutionM solutionInfo projects
            -}
            --load
            return $ InitializerSucceeded out err
            
        ExitFailure _ ->  return $ InitializerFailed out err
