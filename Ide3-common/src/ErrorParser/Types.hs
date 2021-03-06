module ErrorParser.Types where

import Ide3.SrcLoc

newtype ProjectName s = ProjectName { unProjectName :: s } deriving (Read, Show, Eq, Ord)
newtype ModuleName s = ModuleName { unModuleName :: s } deriving (Read, Show, Eq, Ord)

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
