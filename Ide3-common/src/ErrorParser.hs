{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
module ErrorParser 
    ( parseLog
    ) where

import Text.Read (readMaybe)
import Data.List

import Control.Monad
import Control.Monad.State.Strict
import Control.Monad.Trans.Except

import Text.Parsec ((<|>), runParser, ParsecT, Stream)
import qualified Text.Parsec as P

import ErrorParser.Types

import Ide3.SrcLoc

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

libraryLine :: Stream s m Char => ParsecT s u m String
libraryLine = do
    library <- P.string "library"
    void $ P.many P.anyChar
    return library

nonLibraryLine :: Stream s m Char => String -> ParsecT s u m String
nonLibraryLine str = do
    void $ P.string str 
    void $ P.char ' ' 
    void $ P.char '\'' 
    projName <- P.manyTill P.anyChar (P.try $ P.char '\'') 
    void $ P.many P.anyChar 
    return projName

executableLine :: Stream s m Char => ParsecT s u m String
executableLine = nonLibraryLine "executable"

testSuiteLine :: Stream s m Char => ParsecT s u m String
testSuiteLine = nonLibraryLine "test-suite"

benchmarkLine :: Stream s m Char => ParsecT s u m String
benchmarkLine = nonLibraryLine "benchmark"
    
projectLine :: Stream s m Char => ParsecT s u m LineType
projectLine = do
    void $ P.string "Preprocessing "
    liftM (ProjectLine . ProjectName) 
        $   libraryLine 
        <|> executableLine 
        <|> testSuiteLine 
        <|> benchmarkLine

moduleCounter :: Stream s m Char => ParsecT s u m LineType
moduleCounter = do
    (currentStr, totalStr) <- P.between (P.char '[') (P.char ']') $ do
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
    case (readMaybe currentStr, readMaybe totalStr) of
        (Just current, Just total) -> 
            return $ ModuleCounter current total (ModuleName name)
        _ -> error $ "Failed to parse module counter: " 
                   ++ show (currentStr, totalStr, name)

errorType :: Stream s m Char => ParsecT s u m Bool
errorType = (P.try $ P.string " warning:" *> return True) 
        <|> (P.string " error:" *> return False)

counter :: Stream s m Char => ParsecT s u m String
counter = P.many1 P.digit

counterSep :: Stream s m Char => ParsecT s u m ()
counterSep = void $ P.char ':'

lineRemainder :: Stream s m Char => ParsecT s u m ()
lineRemainder = void $ P.manyTill P.anyChar (P.try P.eof)

sourceLocationTail :: Stream s m Char => ParsecT s u m (String, String, Bool)
sourceLocationTail = (,,) <$> (counter   <* counterSep)
                          <*> (counter   <* counterSep)
                          <*> (errorType <* lineRemainder)
    {-
    do
    row <- P.many1 P.digit
    void $ P.char ':'
    col <- P.many1 P.digit
    void $ P.char ':'
    flag <- errorType
    void $ P.manyTill P.anyChar (P.try P.eof)
    return (row,col,flag)
    -}

sourceLocation' :: Stream s m Char => ParsecT s u m ([String],(String,String,Bool))
sourceLocation' = do
    part <- P.manyTill P.anyChar (P.try $ P.char ':')
    --when (null part) $ unexpected ':'
    let left = do
            tailParts <- P.try sourceLocationTail
            return ([part],tailParts)
        right = do
            (parts,tailParts) <- sourceLocation'
            return $ (part:parts,tailParts)
    P.try left <|> right

sourceLocation :: Stream s m Char => ParsecT s u m LineType
sourceLocation = do
    (parts,(rowStr,colStr,flag)) <- sourceLocation'
    let path = intercalate ":" parts
    case (readMaybe rowStr, readMaybe colStr) of
        (Just row, Just col) -> 
            return $ SourceLocation path row col (if flag then IsWarning else IsError)
        _ -> error $ "Failed to parse source location: " 
                   ++ show (parts, rowStr, colStr, flag)

textLine :: Stream s m Char => ParsecT s u m LineType
textLine = TextLine <$> P.many P.anyChar

line :: Stream s m Char => ParsecT s u m LineType
line =  P.try projectLine 
    <|> P.try moduleCounter 
    <|> P.try sourceLocation 
    <|> textLine
    

mkError :: ErrorType 
        -> ProjectName 
        -> ModuleName 
        -> Row 
        -> Column 
        -> [String]
        -> Error ErrorLocation
mkError IsWarning projName modName row col parts 
    = Warning (ErrorLocation projName modName) row col $ unlines $ reverse parts
mkError IsError projName modName row col parts 
    = Error (ErrorLocation projName modName) row col $ unlines $ reverse parts

addError :: MonadState ParserState m
         => ErrorType 
         -> ProjectName 
         -> ModuleName 
         -> Row 
         -> Column
         -> m ()
addError etype projName modName row col = do
    modify $ \s -> s 
        { errors = mkError etype projName modName row col (messageBuffer s) 
                  : errors s
        }
    clearBuffer         

addErrorLine :: MonadState ParserState m
             => String
             -> m ()
addErrorLine errorLine = modify
    $ \s -> s { messageBuffer = errorLine : messageBuffer s
              }

setMode :: MonadState ParserState m
        => ParserMode
        -> m ()
setMode mode' = modify $ \s -> s{ mode = mode' }

popLine :: MonadState ParserState m
        => m (Maybe String)
popLine = do
    result <- gets logLines
    case result of
        [] -> return Nothing
        (x:xs) -> do
            modify $ \s -> s{ logLines = xs }
            return $ Just x

clearBuffer :: MonadState ParserState m => m ()
clearBuffer = modify $ \s -> s{ messageBuffer = [] }

ifNotFinished :: MonadState ParserState m => (ParserMode -> m ()) -> m ()
ifNotFinished f = do
    pmode <- gets mode
    case pmode of
        Finished -> return ()
        _ -> f pmode

parseLine :: MonadState ParserState m => ExceptT P.ParseError m ()
parseLine = ifNotFinished $ \pmode -> do
    result <- lift popLine
    case result of
        Nothing -> do
            case pmode of
                ReadingMessage projName modName row col etype 
                    -> lift $ addError etype projName modName row col
                _ -> return ()
            lift $ setMode Finished
        Just nextLine -> do
            parseResult <- case runParser line () "" nextLine of
                Right x -> return x
                Left err -> throwE err
            lift $ case pmode of
                LookingForProject -> case parseResult of
                    ProjectLine projName -> do
                        setMode $ LookingForModule projName
                    _ -> return ()
                
                LookingForModule projName -> case parseResult of
                    ProjectLine projName' -> do
                        setMode $ LookingForModule projName'
                    ModuleCounter _ _ modName -> do
                        setMode $ LookingForError projName modName
                    _ -> return ()
                
                LookingForError projName modName -> case parseResult of
                    ProjectLine projName' -> do
                        setMode $ LookingForModule projName'
                    ModuleCounter _ _ modName' -> do
                        setMode $ LookingForError projName modName'
                    SourceLocation _ row col etype -> do
                        setMode $ ReadingMessage projName modName row col etype
                    _ -> return ()
                
                ReadingMessage projName modName row col etype -> case parseResult of
                    ProjectLine projName' -> do
                        addError etype projName modName row col
                        setMode $ LookingForModule projName'
                    ModuleCounter _ _ modName' -> do
                        addError etype projName modName row col
                        setMode $ LookingForError projName modName'
                    SourceLocation _ row' col' etype' -> do
                        addError etype projName modName row col
                        setMode $ ReadingMessage projName modName row' col' etype'
                    TextLine "" -> do
                        addError etype projName modName row col
                        setMode $ LookingForError projName modName
                    TextLine text -> do
                        addErrorLine text
                Finished -> undefined
                                        
parseLog :: String -> Maybe [Error ErrorLocation]
parseLog logContents = do
    result <- evalStateT (runExceptT $ loop >> finalize) emptyState
    case result of
        Right errs -> Just errs
        Left _ -> Nothing
  where
    emptyState = ParserState
               { mode = LookingForProject
               , logLines = lines logContents
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
