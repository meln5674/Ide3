{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module ErrorParser 
    ( parseLog
    , Error (..)
    , ErrorType (..)
    , ErrorLocation (..)
    , ProjectName (..)
    , ModuleName (..)
    , Row (..)
    , Column (..)
    , mapError
    , mapErrorM
    ) where

import Prelude hiding (lines, words, unwords, unlines, null)
import qualified Prelude

import Data.String hiding (unlines, lines, null)

import Data.Text (Text)
import qualified Data.Text as T

import Text.Read (readMaybe)
import Data.List hiding (unlines, lines, null)

import Control.Monad
import Control.Monad.State.Strict
import Control.Monad.Trans.Except

import Text.Parsec ((<|>), runParser, runParserT, ParsecT, Stream)
import qualified Text.Parsec as P

import ErrorParser.Types

import Ide3.SrcLoc

class IsString s => IsString' s where
    toString :: s -> String
    lines :: s -> [s]
    unlines :: [s] -> s
    words :: s -> [s]
    unwords :: [s] -> s
    null :: s -> Bool

instance IsString' String where
    toString = id
    lines = Prelude.lines
    unlines = Prelude.unlines
    words = Prelude.words
    unwords = Prelude.unwords
    null = Prelude.null

instance IsString' Text where
    toString = T.unpack
    lines = T.lines
    unlines = T.unlines
    words = T.words
    unwords = T.unwords
    null = T.null

data ErrorType = IsError | IsWarning deriving (Show, Eq)

data ParserMode s
    = LookingForProject
    | LookingForModule !(ProjectName s)
    | LookingForError !(ProjectName s) !(ModuleName s)
    | ReadingMessage !(ProjectName s) !(ModuleName s) !Row !Column !ErrorType
    | Finished

data LineType s
    = ProjectLine !(ProjectName s)
    | ModuleCounter !Int !Int !(ModuleName s)
    | SourceLocation !FilePath !Row !Column !ErrorType
    | TextLine !s
  deriving Show


data ParserState s
    = ParserState
    { mode :: !(ParserMode s)
    , logLines :: ![s]
    , messageBuffer :: ![s]
    , errors :: ![Error (ErrorLocation s) s]
    }

libraryLine :: (IsString' s, Stream s m Char) => ParsecT s u m s
libraryLine = do
    library <- P.string "library"
    void $ P.many P.anyChar
    return $ fromString library

nonLibraryLine :: (IsString' s, Stream s m Char) => String -> ParsecT s u m s
nonLibraryLine str = do
    void $ P.string str 
    void $ P.char ' ' 
    void $ P.char '\'' 
    projName <- P.manyTill P.anyChar (P.try $ P.char '\'') 
    void $ P.many P.anyChar 
    return $ fromString projName

executableLine :: (IsString' s, Stream s m Char) => ParsecT s u m s
executableLine = nonLibraryLine "executable"

testSuiteLine :: (IsString' s, Stream s m Char) => ParsecT s u m s
testSuiteLine = nonLibraryLine "test-suite"

benchmarkLine :: (IsString' s, Stream s m Char) => ParsecT s u m s
benchmarkLine = nonLibraryLine "benchmark"
    
projectLine :: (IsString' s, Stream s m Char) => ParsecT s u m (LineType s)
projectLine = do
    void $ P.string "Preprocessing "
    (ProjectLine . ProjectName) <$> P.choice 
        [ libraryLine 
        , executableLine 
        , testSuiteLine 
        , benchmarkLine
        ]

moduleCounter :: (Show s, IsString' s, Stream s m Char) => ParsecT s u m (LineType s)
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
    name <- fromString <$> P.manyTill P.anyChar (P.try P.space)
    case (readMaybe currentStr, readMaybe totalStr) of
        (Just current, Just total) -> 
            return $ ModuleCounter current total (ModuleName name)
        _ -> error $ "Failed to parse module counter: " 
                   ++ show (currentStr, totalStr, name)

errorType :: (IsString' s, Stream s m Char) => ParsecT s u m Bool
errorType = P.try (P.string " warning:" *> return True) 
        <|> (P.string " error:" *> return False)

counter :: (IsString' s, Stream s m Char) => ParsecT s u m s
counter = fromString <$> P.many1 P.digit

counterSep :: (IsString' s, Stream s m Char) => ParsecT s u m ()
counterSep = void $ P.char ':'

lineRemainder :: (IsString' s, Stream s m Char) => ParsecT s u m ()
lineRemainder = void $ P.manyTill P.anyChar (P.try P.eof)

sourceLocationTail :: (IsString' s, Stream s m Char) => ParsecT s u m (s, s, Bool)
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

sourceLocation' :: (IsString' s, Stream s m Char) => ParsecT s u m ([s],(s,s,Bool))
sourceLocation' = do
    part <- fromString <$> P.manyTill P.anyChar (P.try $ P.char ':')
    --when (null part) $ unexpected ':'
    let left = do
            tailParts <- P.try sourceLocationTail
            return ([part],tailParts)
        right = do
            (parts,tailParts) <- sourceLocation'
            return (part:parts, tailParts)
    P.try left <|> right

sourceLocation :: (Show s, IsString' s, Stream s m Char) => ParsecT s u m (LineType s)
sourceLocation = do
    (parts,(rowStr,colStr,flag)) <- sourceLocation'
    let path = intercalate ":" $ map toString parts
    case (readMaybe $ toString rowStr, readMaybe $ toString colStr) of
        (Just row, Just col) -> 
            return $ SourceLocation path row col (if flag then IsWarning else IsError)
        _ -> error $ "Failed to parse source location: " 
                   ++ show (parts, rowStr, colStr, flag)

textLine :: (IsString' s, Stream s m Char) => ParsecT s u m (LineType s)
textLine = (TextLine . fromString) <$> P.many P.anyChar

line :: (Show s, IsString' s, Stream s m Char) => ParsecT s u m (LineType s)
line =  P.try projectLine 
    <|> P.try moduleCounter 
    <|> P.try sourceLocation 
    <|> textLine
    

mkError :: (IsString' s)
        => ErrorType 
        -> ProjectName s
        -> ModuleName s
        -> Row 
        -> Column 
        -> [s]
        -> Error (ErrorLocation s) s
mkError IsWarning projName modName row col parts 
    = Warning (ErrorLocation projName modName) row col $ unlines $ reverse parts
mkError IsError projName modName row col parts 
    = Error (ErrorLocation projName modName) row col $ unlines $ reverse parts

addError :: (IsString' s, MonadState (ParserState s) m)
         => ErrorType 
         -> ProjectName s
         -> ModuleName s
         -> Row 
         -> Column
         -> m ()
addError etype projName modName row col = do
    modify $ \s -> s 
        { errors = mkError etype projName modName row col (messageBuffer s) 
                  : errors s
        }
    clearBuffer         

addErrorLine :: MonadState (ParserState s) m
             => s
             -> m ()
addErrorLine errorLine = modify
    $ \s -> s { messageBuffer = errorLine : messageBuffer s
              }

setMode :: MonadState (ParserState s) m
        => ParserMode s
        -> m ()
setMode mode' = modify $ \s -> s{ mode = mode' }

popLine :: MonadState (ParserState s) m
        => m (Maybe s)
popLine = do
    result <- gets logLines
    case result of
        [] -> return Nothing
        (x:xs) -> do
            modify $ \s -> s{ logLines = xs }
            return $ Just x

clearBuffer :: MonadState (ParserState s) m => m ()
clearBuffer = modify $ \s -> s{ messageBuffer = [] }

ifNotFinished :: (IsString' s, MonadState (ParserState s) m) => (ParserMode s -> m ()) -> m ()
ifNotFinished f = do
    pmode <- gets mode
    case pmode of
        Finished -> return ()
        _ -> f pmode

processLine :: ( Show s
               , IsString' s
               , MonadState (ParserState s) m
               , Stream s m Char
               )
            => s
            -> ParserMode s
            -> ExceptT P.ParseError m ()
processLine nextLine pmode = do
    parseResult <- ExceptT $ runParserT line () "" nextLine
    lift $ case pmode of
        LookingForProject -> case parseResult of
            ProjectLine projName -> 
                setMode $ LookingForModule projName
            _ -> return ()
        
        LookingForModule projName -> case parseResult of
            ProjectLine projName' -> 
                setMode $ LookingForModule projName'
            ModuleCounter _ _ modName -> 
                setMode $ LookingForError projName modName
            _ -> return ()
        
        LookingForError projName modName -> case parseResult of
            ProjectLine projName' -> 
                setMode $ LookingForModule projName'
            ModuleCounter _ _ modName' -> 
                setMode $ LookingForError projName modName'
            SourceLocation _ row col etype -> 
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
            TextLine text
                | null text -> do
                    addError etype projName modName row col
                    setMode $ LookingForError projName modName
                | otherwise -> addErrorLine text
        Finished -> undefined

parseLine :: ( Show s
             , IsString' s
             , MonadState (ParserState s) m
             , Stream s m Char
             )
          => ExceptT P.ParseError m ()
parseLine = ifNotFinished $ \pmode -> do
    result <- lift popLine
    case result of
        Nothing -> do
            case pmode of
                ReadingMessage projName modName row col etype 
                    -> lift $ addError etype projName modName row col
                _ -> return ()
            lift $ setMode Finished
        Just nextLine -> processLine nextLine pmode
            
                                        
parseLog :: ( Show s
            , IsString' s
            , Monad m
            , Stream s (StateT (ParserState s) m) Char
            )
         => s 
         -> m (Maybe [Error (ErrorLocation s) s])
parseLog logContents = do
    result <- evalStateT (runExceptT $ loop >> finalize) emptyState
    case result of
        Right errs -> return $ Just errs
        Left _ -> return Nothing
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
