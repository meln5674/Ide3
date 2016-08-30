{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
module ErrorParser where

import Text.Read (readMaybe)
import Data.List

import Control.Monad
import Control.Monad.State.Strict
import Control.Monad.Trans.Except

import Text.Parsec (Parsec, (<|>), runParser)
import qualified Text.Parsec as P
import qualified Text.Parsec.Char as P

import ErrorParser.Types


data ErrorType = IsError | IsWarning deriving (Show, Eq)

data ParserMode
    = LookingForProject
    | LookingForModule !ProjectName
    | LookingForError !ProjectName !ModuleName
    | ReadingMessage !ProjectName !ModuleName !Row !Column !ErrorType
    | Finished

data LineType
    = ProjectLine !ProjectName
    | ModuleCounter !Int !Int !ModuleName
    | SourceLocation !FilePath !Row !Column !ErrorType
    | TextLine !String
  deriving Show


data ParserState
    = ParserState
    { mode :: !ParserMode
    , logLines :: ![String]
    , messageBuffer :: ![String]
    , errors :: ![Error ErrorLocation]
    }

libraryLine = do
    library <- P.string "library"
    P.many P.anyChar
    return library

nonLibraryLine str = do
    P.string str 
    P.char ' ' 
    P.char '\'' 
    projName <- P.manyTill P.anyChar (P.try $ P.char '\'') 
    P.many P.anyChar 
    return projName

executableLine = nonLibraryLine "executable"
testSuiteLine = nonLibraryLine "test-suite"
benchmarkLine = nonLibraryLine "benchmark"
    

projectLine = do
    void $ P.string "Preprocessing "
    liftM (ProjectLine . ProjectName) 
        $   libraryLine 
        <|> executableLine 
        <|> testSuiteLine 
        <|> benchmarkLine

moduleCounter = do
    (current, total) <- P.between (P.char '[') (P.char ']') $ do
        P.skipMany P.space
        current <- P.many1 P.digit
        P.skipMany P.space
        void $ P.string "of"
        P.skipMany P.space
        total <- P.many1 P.digit
        P.skipMany P.space
        return (current, total)
    void $ P.string " Compiling "
    name <- P.manyTill P.anyChar (P.try P.space)
    case (readMaybe current, readMaybe total) of
        (Just current, Just total) -> 
            return $ ModuleCounter current total (ModuleName name)
        _ -> error $ "Failed to parse module counter: " ++ show (current, total, name)

sourceLocationTail = do
    row <- P.many1 P.digit
    void $ P.char ':'
    col <- P.many1 P.digit
    void $ P.char ':'
    flag <- P.option False (P.string " Warning:" *> return True)
    P.eof
    return (row,col,flag)

sourceLocation' = do
    part <- P.manyTill P.anyChar (P.try $ P.char ':')
    --when (null part) $ unexpected ':'
    let left = do
            tail <- P.try sourceLocationTail
            return ([part],tail)
        right = do
            (parts,tail) <- sourceLocation'
            return $ (part:parts,tail)
    P.try left <|> right

sourceLocation = do
    (parts,(row,col,flag)) <- sourceLocation'
    let path = intercalate ":" parts
    case (readMaybe row, readMaybe col) of
        (Just row, Just col) -> 
            return $ SourceLocation path row col (if flag then IsWarning else IsError)
        _ -> error $ "Failed to parse source location: " ++ show (parts, row, col, flag)

textLine = TextLine <$> P.many P.anyChar

line = P.try projectLine <|> P.try moduleCounter <|> P.try sourceLocation <|> textLine
    
    
mkError IsWarning projName modName row col parts = Warning (ErrorLocation projName modName) row col $ unlines $ reverse parts
mkError IsError projName modName row col parts = Error (ErrorLocation projName modName) row col $ unlines $ reverse parts

addError etype projName modName row col
    = modify 
    $ \s -> s { errors = mkError etype projName modName row col (messageBuffer s) : errors s
              , messageBuffer = []
              }

addErrorLine errorLine = modify
    $ \s -> s { messageBuffer = errorLine : messageBuffer s
              }

setMode mode' = modify $ \s -> s{ mode = mode' }

popLine = do
    result <- gets logLines
    case result of
        [] -> return Nothing
        (x:xs) -> do
            modify $ \s -> s{ logLines = xs }
            return $ Just x

clearBuffer = modify $ \s -> s{ messageBuffer = [] }

ifNotFinished f = do
    pmode <- lift $ gets mode
    case pmode of
        Finished -> return ()
        _ -> f pmode

parseLine = ifNotFinished $ \pmode -> do
    result <- lift popLine
    case result of
        Nothing -> do
            case pmode of
                ReadingMessage projName modName row col etype -> lift $ addError etype projName modName row col
                _ -> return ()
            lift $ setMode Finished
        Just nextLine -> do
            result <- case runParser line () "" nextLine of
                Right x -> return x
                Left err -> throwE err
            lift $ case pmode of
                LookingForProject -> case result of
                    ProjectLine projName -> do
                        setMode $ LookingForModule projName
                    _ -> return ()
                
                LookingForModule projName -> case result of
                    ProjectLine projName' -> do
                        setMode $ LookingForModule projName'
                    ModuleCounter _ _ modName -> do
                        setMode $ LookingForError projName modName
                    _ -> return ()
                
                LookingForError projName modName -> case result of
                    ProjectLine projName' -> do
                        setMode $ LookingForModule projName'
                    ModuleCounter _ _ modName' -> do
                        setMode $ LookingForError projName modName'
                    SourceLocation _ row col etype -> do
                        setMode $ ReadingMessage projName modName row col etype
                    _ -> return ()
                
                ReadingMessage projName modName row col etype -> case result of
                    ProjectLine projName' -> do
                        addError etype projName modName row col
                        setMode $ LookingForModule projName'
                    ModuleCounter _ _ modName' -> do
                        addError etype projName modName row col
                        setMode $ LookingForError projName modName'
                    SourceLocation path' row' col' etype' -> do
                        addError etype projName modName row col
                        setMode $ ReadingMessage projName modName row' col' etype'
                    TextLine "" -> do
                        addError etype projName modName row col
                        setMode $ LookingForError projName modName
                    TextLine text -> do
                        addErrorLine text
                        
parseLog :: String -> Maybe [Error ErrorLocation]
parseLog log = do
    result <- evalStateT (runExceptT $ loop >> finalize) emptyState
    case result of
        Right errs -> Just errs
        Left _ -> Nothing
  where
    emptyState = ParserState
               { mode = LookingForProject
               , logLines = lines log
               , messageBuffer = []
               , errors = []
               }
    loop = do
        parseLine
        pmode <- gets mode
        case pmode of
            Finished -> return()
            _ -> loop
    finalize = do
        errorList <- gets errors
        return $ reverse errorList
