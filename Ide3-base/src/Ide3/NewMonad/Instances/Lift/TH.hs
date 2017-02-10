module Ide3.NewMonad.Instances.Lift.TH where

import Data.Map (Map)
import qualified Data.Map as M

import Control.Monad
import Control.Monad.Trans

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

data BetterderivingOverride = BetterderivingOverride TH.Name TH.Name

data Betterderiving
    = Betterderiving
    { betterderivingDefaultLiftMethod :: TH.Name
    , betterderivingSupers :: [BetterderivingHead]
    , betterderivingClassName :: [TH.Name]
    , betterderivingLift :: BetterderivingHead
    , betterderivingOverrides :: Map TH.Name TH.Name
    }

head2Pred :: BetterderivingHead -> TH.Pred
head2Pred (BetterderivingHead cname args) = foldl app (TH.ConT cname) args
  where
    app t x = TH.AppT t $ TH.VarT x

commaWithSpaces :: ParsecT String u TH.Q ()
commaWithSpaces = spaces *> char ',' *> spaces

lookupTypeNameOrFail :: String -> TH.Q TH.Name
lookupTypeNameOrFail s = do
    result <- TH.lookupTypeName s
    case result of
        Just n -> return n
        Nothing -> fail $ "Could not find type name " ++ s

lookupValueNameOrFail :: String -> TH.Q TH.Name
lookupValueNameOrFail s = do
    result <- TH.lookupValueName s
    case result of
        Just n -> return n
        Nothing -> fail $ "Could not find value name " ++ s

identChar :: ParsecT String u TH.Q Char
identChar = alphaNum <|> char '\'' 
                     <|> char '_'
                     <|> char '.'

tyCon :: ParsecT String u TH.Q TH.Name
tyCon = lift . lookupTypeNameOrFail =<<
         ( (:) <$> upper 
               <*> many identChar
         )

tyVar :: ParsecT String u TH.Q TH.Name
tyVar = lift . lookupTypeNameOrFail =<<
        ( (:) <$> lower 
              <*> many identChar
        )

idCon :: ParsecT String u TH.Q TH.Name
idCon = lift . lookupValueNameOrFail =<<
        ( (:) <$> upper 
              <*> many identChar
        )

idVar :: ParsecT String u TH.Q TH.Name
idVar = lift . lookupValueNameOrFail =<<
        ( (:) <$> lower 
              <*> many identChar
        )

localCon :: ParsecT String u TH.Q TH.Name
localCon = TH.mkName
    <$> ( (:) <$> upper 
              <*> many identChar
        )

localVar :: ParsecT String u TH.Q TH.Name
localVar = TH.mkName
    <$> ( (:) <$> lower 
              <*> many identChar
        )


mkOverrideMap :: [BetterderivingOverride] -> Map TH.Name TH.Name
mkOverrideMap = M.fromList . map toPair
 where
   toPair (BetterderivingOverride methodName liftFunc) = (methodName, liftFunc)

betterderivingHead :: ParsecT String u TH.Q BetterderivingHead
betterderivingHead = BetterderivingHead <$> tyCon 
                                        <*> (spaces *> sepBy localVar spaces)

betterderivingOverride :: ParsecT String u TH.Q BetterderivingOverride
betterderivingOverride = BetterderivingOverride <$> idVar <*> (spaces *> char '=' *> spaces *> idVar)

betterderivingBody :: ParsecT String u TH.Q Betterderiving
betterderivingBody 
    = Betterderiving <$> idVar
                     <*> (endOfBodyLine *> sepBy betterderivingHead (try commaWithSpaces))
                     <*> (endOfBodyLine *> sepBy tyCon commaWithSpaces)
                     <*> (endOfBodyLine *> betterderivingHead)
                     <*> (mkOverrideMap <$> (endOfBodyLine *> sepBy betterderivingOverride (try commaWithSpaces)))

endOfBodyLine :: ParsecT String u TH.Q ()
endOfBodyLine = spaces *> char ';' *> spaces

parseBetterderiving :: (SourceName, Line, Column) -> String -> TH.Q Betterderiving
parseBetterderiving (file, line, col) s = do
    result <- runParserT p () "" s
    case result of
        Left err -> fail $ show err
        Right e -> return e
  where
    --p :: ParsecT s u TH.Q Betterderiving
    p = do
        pos <- getPosition
        setPosition $ flip setSourceName file
                    $ flip setSourceLine line
                    $ flip setSourceColumn col
                      pos
        spaces
        b <- betterderivingBody
        spaces
        eof
        return b
            
quoteBetterderivingDec :: String -> TH.Q [TH.Dec]
quoteBetterderivingDec s = do
    loc <- TH.location
    let pos = ( TH.loc_filename loc
              , fst $ TH.loc_start loc
              , snd $ TH.loc_start loc
              )
    Betterderiving defaultLiftFunc supers classNames (BetterderivingHead conName conArgs) overrideMap <- parseBetterderiving pos s
    let getLiftFunc methodName = maybe defaultLiftFunc id (M.lookup methodName overrideMap)
    forM classNames $ \className ->
        mkSpliceInstance getLiftFunc (map head2Pred supers) className conName conArgs
        
mkConApp :: TH.Name -> [TH.Type] -> TH.Type
mkConApp n = foldl TH.AppT (TH.ConT n)

modifyLast :: (a -> a) -> [a] -> [a]
modifyLast _ [] = error "Empty list"
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
appArgs = foldl app
  where
    app t x = TH.AppE t $ TH.VarE x

appTArgs :: TH.Type -> [TH.Type] -> TH.Type
appTArgs = foldl TH.AppT

mkSpliceInstanceMethod' :: TH.Name -> TH.Name -> TH.Type -> TH.Q TH.Dec
mkSpliceInstanceMethod' liftFunc methodName methodType = do
    let argCount = funcArgCount methodType
    varNames <- replicateM argCount $ TH.newName "x"
    let pats = map TH.VarP varNames
    let body = TH.NormalB $ TH.AppE (TH.VarE liftFunc) 
                          $ TH.ParensE 
                          $ appArgs (TH.VarE methodName) varNames
    return $ TH.FunD methodName [TH.Clause pats body []]

mkSpliceInstanceMethod :: (TH.Name -> TH.Name) -> TH.Dec -> TH.Q TH.Dec
mkSpliceInstanceMethod getLiftFunc (TH.FunD methodName _) = do
    let liftFunc = getLiftFunc methodName
    result <- TH.reify methodName
    methodType <- case result of
        (TH.ClassOpI _ methodType _) -> return methodType
        x -> fail $ "Couldn't reify method, got " ++ show x
    mkSpliceInstanceMethod' liftFunc methodName methodType
mkSpliceInstanceMethod getLiftFunc (TH.SigD methodName methodType) = mkSpliceInstanceMethod' (getLiftFunc methodName) methodName methodType
mkSpliceInstanceMethod _ x = fail $ "Couldn't reify method, got " ++ show x

mkSpliceInstanceType :: TH.Name -> [TH.Name] -> TH.TypeFamilyHead -> TH.Q TH.Dec
mkSpliceInstanceType conName conVars (TH.TypeFamilyHead typeName typeVars _ _) = do
    let typeType = TH.ConT typeName
        typeVarNames = map getBndrName typeVars
        varTypes = map TH.VarT typeVarNames
        lhsArgs = spliceConName conName conVars varTypes
        rhs = appTArgs typeType varTypes
    return $ TH.TySynInstD typeName $ TH.TySynEqn lhsArgs rhs

mkSpliceInstanceItem :: (TH.Name -> TH.Name) -> TH.Name -> [TH.Name] -> TH.Dec -> TH.Q TH.Dec
mkSpliceInstanceItem _ conName conVars (TH.OpenTypeFamilyD familyHead) = mkSpliceInstanceType conName conVars familyHead
mkSpliceInstanceItem getLiftFunc _ _ dec@TH.FunD{} = mkSpliceInstanceMethod getLiftFunc dec
mkSpliceInstanceItem getLiftFunc _ _ dec@TH.SigD{} = mkSpliceInstanceMethod getLiftFunc dec
mkSpliceInstanceItem _ _ _ x = fail $ "Unexpected class element " ++ show x


getBndrName :: TH.TyVarBndr -> TH.Name
getBndrName (TH.PlainTV n) = n
getBndrName (TH.KindedTV n _) = n

spliceConName :: TH.Name -> [TH.Name] -> [TH.Type] -> [TH.Type]
spliceConName conName conVars = modifyLast (\v -> mkConApp conName $ map TH.VarT conVars ++ [v])

mkSpliceInstance :: (TH.Name -> TH.Name) -> TH.Cxt -> TH.Name -> TH.Name -> [TH.Name] -> TH.Q TH.Dec
mkSpliceInstance getLiftFunc conCxt className conName conVars = do
    (TH.ClassI (TH.ClassD _ _ classVarBinders _ classItems) _) <- TH.reify className
    let overlap = Nothing
        varNames = map getBndrName classVarBinders
        varTypes = map TH.VarT varNames
        cxt = mkConApp className varTypes : conCxt
        instVars = spliceConName conName conVars varTypes
        instType = appTArgs (TH.ConT className) instVars
    instMethods <- mapM (mkSpliceInstanceItem getLiftFunc conName conVars) classItems
    return $ TH.InstanceD overlap cxt instType instMethods



