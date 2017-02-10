module GuiClass.Dialogs where

instance (Monad m, MonadIO m') => ProjectInitializerClass (DialogsT {-proxy-} m p m') where
    type ClassProjectInitializerMonad (DialogsT {-proxy-} m p m') = m'
    setupProjectCreator onConfirm
        = id {- access the project creator dialog #-}
        $ lift $ liftIO $ do
            {- attach `onConfirm` to the confirm clicked event of the project creator dialog -}
            return ()
            
    getProjectCreatorArg
        = withGuiComponents
        $ const {- access some kind of buffer #-}
        $ lift $ liftIO $ do
            {- get the contents of the buffer, create a stack args -}
            return $ Left $ Unsupported "Project creation"
        
        
    finalizeProjectCreator 
        = id {- access the project creator dialog -}
        $ lift $ liftIO $ do
            {- close the dialog -}
            return ()

instance ( MonadIO m
         , ViewerMonad m
         , ArgType m' ~ StackInitializerArgs
         ) => SolutionInitializerClass (DialogsT m' p m) where
    type ClassSolutionInitializerMonad (DialogsT {-proxy-} m' p m) = m'
    setupSolutionCreator
        = withNewSolutionDialog
        $ \dialog -> NewSolutionDialog.setVisible dialog True
    getSolutionCreatorArg
        = withNewSolutionDialog
        $ \dialog -> do
            projectRoot <- NewSolutionDialog.getSelectedFolder dialog
            projectName <- unpack <$> NewSolutionDialog.getSolutionName dialog
            templateName <- fmap unpack <$> NewSolutionDialog.getTemplateName dialog
            runExceptT $ case projectRoot of
                Nothing -> throwE $ InvalidOperation "Please choose a directory" ""
                Just projectRoot -> do
                    wrapIOError $ setCurrentDirectory projectRoot
                    bounce $ setDirectoryToOpen $ projectRoot </> projectName
                    return $ StackInitializerArgs projectName templateName
        
        
    finalizeSolutionCreator 
        = withNewSolutionDialog
        $ \dialog -> NewSolutionDialog.setVisible dialog False



