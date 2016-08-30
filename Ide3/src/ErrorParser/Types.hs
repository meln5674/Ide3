module ErrorParser.Types where

newtype Row = Row { getRow :: Int }
newtype Column = Column { getColumn :: Int }

instance Show Row where
    show = show . getRow

instance Show Column where
    show = show . getColumn

instance Read Row where
    readsPrec i s = flip map (readsPrec i s) $ \(a,s) -> (Row a,s) 
    
instance Read Column where
    readsPrec i s = flip map (readsPrec i s) $ \(a,s) -> (Column a,s) 

newtype ProjectName = ProjectName { getProjectName :: String } deriving (Read, Show)
newtype ModuleName = ModuleName { getModuleName :: String } deriving (Read, Show)

data ErrorLocation = ErrorLocation !ProjectName !ModuleName deriving Show

data Error loc
    = Error 
    { errorLocation :: !loc
    , errorRow :: !Row
    , errorColumn :: !Column 
    , errorMessage :: !String
    }
    | Warning 
    { errorLocation :: !loc 
    , errorRow :: !Row 
    , errorColumn :: !Column 
    , errorMessage :: !String
    }
  deriving Show

mapError :: (loc -> Row -> Column -> String -> (loc', Row, Column, String)) 
         -> Error loc
         -> Error loc'
mapError f (Warning loc r c s) = let (loc', r', c', s') = f loc r c s in Warning loc' r' c' s'
mapError f (Error loc r c s) = let (loc', r', c', s') = f loc r c s in Error loc' r' c' s'

mapErrorM :: Monad m
          => (loc -> Row -> Column -> String -> m (loc', Row, Column, String)) 
          -> Error loc
          -> m (Error loc')
mapErrorM f (Warning loc r c s) = do
    (loc', r', c', s') <- f loc r c s
    return $ Warning loc' r' c' s'
mapErrorM f (Error loc r c s) = do
    (loc', r', c', s') <- f loc r c s
    return $ Error loc' r' c' s'
