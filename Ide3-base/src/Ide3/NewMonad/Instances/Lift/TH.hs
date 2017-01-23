{-# LANGUAGE QuasiQuotes #-}
module Ide3.NewMonad.Instances.Lift.TH where

import Control.Monad

import Text.Parsec
import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Quote

betterderiving :: QuasiQuoter
betterderiving = QuasiQuoter { quoteDec = quoteBetterderivingDec
                             , quoteExp = notHandled "expressions"
                             , quotePat = notHandled "patterns"
                             , quoteType = notHandled "types"
                             }
  where
    notHandled things = error $ things ++ " are not handled by betterderiving"

data BetterderivingHead = BetterderivingHead TH.Name [TH.Name]

data BetterderivingMethod = BetterderivingMethod TH.Name Int

data Betterderiving
    = Betterderiving
    { betterderivingLiftMethod :: TH.Name
    , betterderivingSupers :: [BetterderivingHead]
    , betterderivingClassName :: [TH.Name]
    , betterderivingLift :: BetterderivingHead
    }

head2Pred :: BetterderivingHead -> TH.Pred
head2Pred (BetterderivingHead cname args) = go (TH.ConT cname) args
  where
    go t [] = t
    go t (x:xs) = go (TH.AppT t (TH.VarT x)) xs
  

commaWithSpaces = spaces *> char ',' *> spaces

identChar = alphaNum <|> char '\'' 
                     <|> char '_'
                     <|> char '.'

tyCon = TH.mkName <$> ( (:) <$> upper 
                            <*> many identChar
                   )

tyVar = TH.mkName <$> ( (:) <$> lower 
                            <*> many identChar
                   )

--betterderivingHead :: ParsecT s u TH.Q BetterderivingHead
betterderivingHead = BetterderivingHead <$> tyCon 
                                        <*> (spaces *> sepBy tyVar spaces)

--betterderivingBody :: ParsecT s u TH.Q Betterderiving
betterderivingBody 
    = Betterderiving <$> tyVar
                     <*> (endOfBodyLine *> sepBy betterderivingHead (try commaWithSpaces))
                     <*> (endOfBodyLine *> sepBy tyCon commaWithSpaces)
                     <*> (endOfBodyLine *> betterderivingHead <* endOfBodyLine)

endOfBodyLine = spaces *> char ';' *> spaces

--parseBetterderiving :: (SourceName, Line, Column) -> s -> TH.Q Betterderiving
parseBetterderiving (file, line, col) s = do
    result <- runParserT p () "" s
    case result of
        Left err -> fail $ show err
        Right e -> return e
  where
    --p :: ParsecT s u TH.Q Betterderiving
    p = do
        pos <- getPosition
        setPosition $ (flip setSourceName) file
                    $ (flip setSourceLine) line
                    $ (flip setSourceColumn) col
                    $ pos
        spaces
        b <- betterderivingBody
        spaces
        eof
        return b
            

quoteBetterderivingDec s = do
    loc <- TH.location
    let pos = ( TH.loc_filename loc
              , fst $ TH.loc_start loc
              , snd $ TH.loc_start loc
              )
    Betterderiving liftFunc supers classNames (BetterderivingHead conName conArgs) <- parseBetterderiving pos s
    
    instDecs <- forM classNames $ \className -> do
        mkSpliceInstance liftFunc (map head2Pred supers) className conName conArgs
    return instDecs
        
mkConApp :: TH.Name -> [TH.Type] -> TH.Type
mkConApp n = go (TH.ConT n)
  where
    go t [] = t
    go t (x:xs) = go (TH.AppT t x) xs

modifyLast :: (a -> a) -> [a] -> [a]
modifyLast f [] = error "Empty list"
modifyLast f [x] = [f x]
modifyLast f (x:xs) = x : modifyLast f xs

funcArgCount :: TH.Type -> Int
funcArgCount = go 0
  where
    go x (TH.AppT TH.ArrowT t) = go (x+1) t
    go x (TH.AppT (TH.AppT TH.ArrowT _) t) = go (x+1) t
    go x (TH.ForallT _ _ t) = go x t
    go x _ = x

appArgs :: TH.Exp -> [TH.Name] -> TH.Exp
appArgs t [] = t
appArgs t (x:xs) = appArgs (TH.AppE t (TH.VarE x)) xs

appTArgs :: TH.Type -> [TH.Type] -> TH.Type
appTArgs t [] = t
appTArgs t (x:xs) = appTArgs (TH.AppT t x) xs

mkSpliceInstanceMethod' :: TH.Name -> TH.Name -> TH.Type -> TH.Q TH.Dec
mkSpliceInstanceMethod' liftFunc methodName methodType = do
    let argCount = funcArgCount methodType
    varNames <- replicateM argCount $ TH.newName "x"
    let pats = map TH.VarP varNames
    let body = TH.NormalB $ TH.AppE (TH.VarE liftFunc) 
                          $ TH.ParensE 
                          $ appArgs (TH.VarE methodName) varNames
    return $ TH.FunD methodName [TH.Clause pats body []]

mkSpliceInstanceMethod :: TH.Name -> TH.Dec -> TH.Q TH.Dec
mkSpliceInstanceMethod liftFunc (TH.FunD methodName _) = do
    result <- TH.reify methodName
    methodType <- case result of
        (TH.ClassOpI _ methodType _) -> return methodType
        x -> fail $ "Couldn't reify method, got " ++ show x
    mkSpliceInstanceMethod' liftFunc methodName methodType
mkSpliceInstanceMethod liftFunc (TH.SigD methodName methodType) = mkSpliceInstanceMethod' liftFunc methodName methodType
mkSpliceInstanceMethod _ x = fail $ "Couldn't reify method, got " ++ show x

mkSpliceInstance :: TH.Name -> TH.Cxt -> TH.Name -> TH.Name -> [TH.Name] -> TH.Q TH.Dec
mkSpliceInstance liftFunc conCxt className conName conVars = do
    (TH.ClassI (TH.ClassD _ _ classVarBinders deps methods) _) <- TH.reify className
    let overlap = Nothing
        varNames = map getBndrName classVarBinders
        varTypes = map TH.VarT varNames
        cxt = mkConApp className varTypes : conCxt
        getBndrName (TH.PlainTV n) = n
        getBndrName (TH.KindedTV n _) = n
        instVars = modifyLast (\v -> mkConApp conName $ map TH.VarT conVars ++ [v]) varTypes
        instType = appTArgs (TH.ConT className) instVars
    instMethods <- mapM (mkSpliceInstanceMethod liftFunc) methods
    return $ TH.InstanceD overlap cxt instType instMethods


