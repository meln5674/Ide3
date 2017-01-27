module ErrorParser.Types where

import Ide3.SrcLoc

{-
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
-}

newtype ProjectName s = ProjectName { getProjectName :: s } deriving (Read, Show, Eq, Ord)
newtype ModuleName s = ModuleName { getModuleName :: s } deriving (Read, Show, Eq, Ord)

data ErrorLocation s = ErrorLocation !(ProjectName s) !(ModuleName s) deriving (Show, Eq, Ord)

data Error loc s
    = Error 
    { errorLocation :: !loc
    , errorRow :: !Row
    , errorColumn :: !Column 
    , errorMessage :: !s
    }
    | Warning 
    { errorLocation :: !loc 
    , errorRow :: !Row 
    , errorColumn :: !Column 
    , errorMessage :: !s
    }
  deriving Show

mapError :: (loc -> Row -> Column -> s -> (loc', Row, Column, s')) 
         -> Error loc s
         -> Error loc' s'
mapError f (Warning loc r c s) = let (loc', r', c', s') = f loc r c s in Warning loc' r' c' s'
mapError f (Error loc r c s) = let (loc', r', c', s') = f loc r c s in Error loc' r' c' s'

mapErrorM :: Monad m
          => (loc -> Row -> Column -> s -> m (loc', Row, Column, s')) 
          -> Error loc s
          -> m (Error loc' s')
mapErrorM f (Warning loc r c s) = do
    (loc', r', c', s') <- f loc r c s
    return $ Warning loc' r' c' s'
mapErrorM f (Error loc r c s) = do
    (loc', r', c', s') <- f loc r c s
    return $ Error loc' r' c' s'
