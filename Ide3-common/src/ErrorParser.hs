{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module ErrorParser 
    ( ParseToken
    , startParseLog
    , parseNextLogLine
    , log
    , project
    ) where

import Prelude hiding (lines, words, unwords, unlines, null, log)
import qualified Prelude

import Data.String hiding (unlines, lines, null)

import Data.Text (Text)
import qualified Data.Text as T

import Data.Attoparsec.Text (Parser, (<?>))
import qualified Data.Attoparsec.Combinator as P
import qualified Data.Attoparsec.Text as P

import Text.Read (readMaybe)
import Data.List hiding (unlines, lines, null)

import Control.Monad
import Control.Monad.State.Strict
import Control.Monad.Trans.Except

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

type StateParser s = StateT (ParserState s) Parser

libraryLine :: Parser Text
libraryLine = do
    library <- P.string "library"
    void $ P.manyTill' P.anyChar P.endOfLine
    return library

nonLibraryLine :: Text -> Parser Text
nonLibraryLine str = do
    void $ P.string str 
    void $ P.char ' ' 
    void $ P.char '\'' 
    projName <- P.manyTill P.anyChar (P.try $ P.char '\'') 
    void $ P.manyTill' P.anyChar P.endOfLine
    return $ fromString projName

executableLine :: Parser Text
executableLine = nonLibraryLine "executable"

testSuiteLine :: Parser Text
testSuiteLine = nonLibraryLine "test-suite"

benchmarkLine :: Parser Text
benchmarkLine = nonLibraryLine "benchmark"
    
projectLine :: Parser (LineType Text)
projectLine = do
    void $ P.string "Preprocessing "
    liftM (ProjectLine . ProjectName) 
        $   P.choice
                [ libraryLine 
                , executableLine 
                , testSuiteLine 
                , benchmarkLine
                ]

moduleCounter :: Parser (LineType Text)
moduleCounter = do
    P.char '['
    P.skipMany P.space
    currentStr <- P.many1 P.digit
    P.skipMany P.space
    void $ P.string "of"
    P.skipMany P.space
    totalStr <- P.many1 P.digit
    P.skipMany P.space
    P.char ']'
    void $ P.string " Compiling "
    name <- fromString <$> P.manyTill P.anyChar (P.try P.space)
    case (readMaybe currentStr, readMaybe totalStr) of
        (Just current, Just total) -> do
            lineRemainder
            return $ ModuleCounter current total (ModuleName name)
        _ -> error $ "Failed to parse module counter: " 
                   ++ show (currentStr, totalStr, name)

errorType :: Parser Bool
errorType = P.choice
    [ (P.try $ P.string " warning:" *> return True) 
    , (P.string " error:" *> return False)
    ]

counter :: Parser Text
counter = fst <$> P.match P.decimal

counterSep :: Parser ()
counterSep = void $ P.char ':'

lineRemainder :: Parser ()
lineRemainder = void $ P.manyTill P.anyChar P.endOfLine

sourceLocationTail :: Parser (Text, Text, Bool)
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

sourceLocation' :: Parser ([Text],(Text,Text,Bool))
sourceLocation' = do
    part <- fromString <$> P.manyTill (P.satisfy $ not . P.isEndOfLine) (P.try $ P.char ':')
    --when (null part) $ unexpected ':'
    let left = do
            tailParts <- P.try sourceLocationTail
            return ([part],tailParts)
        right = do
            (parts,tailParts) <- sourceLocation'
            return $ (part:parts,tailParts)
    P.try $ P.choice [left, right]

sourceLocation :: Parser (LineType Text)
sourceLocation = do
    (parts,(rowStr,colStr,flag)) <- sourceLocation'
    let path = intercalate ":" $ map toString parts
    case (readMaybe $ toString rowStr, readMaybe $ toString colStr) of
        (Just row, Just col) -> 
            return $ SourceLocation path row col (if flag then IsWarning else IsError)
        _ -> error $ "Failed to parse source location: " 
                   ++ show (parts, rowStr, colStr, flag)

textLine :: Parser (LineType Text)
textLine = (TextLine . fromString) <$> P.manyTill' P.anyChar P.endOfLine

line :: Parser (LineType Text)
line =  P.choice
    [ P.try projectLine 
    , P.try moduleCounter 
    , P.try sourceLocation 
    , textLine
    ]
    

mkError :: ErrorType 
        -> (ProjectName Text)
        -> (ModuleName Text)
        -> Row 
        -> Column 
        -> [Text]
        -> Error (ErrorLocation Text) Text
mkError IsWarning projName modName row col parts 
    = Warning (ErrorLocation projName modName) row col $ unlines $ reverse parts
mkError IsError projName modName row col parts 
    = Error (ErrorLocation projName modName) row col $ unlines $ reverse parts

addError :: (MonadState (ParserState Text) m)
         => ErrorType 
         -> (ProjectName Text)
         -> (ModuleName Text)
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

type ParseError = String

{-
processLine :: ( Show s
               , IsString' s
               )
            => ParserMode s
            -> StateParser s ()
processLine pmode = do
    parseResult <- line
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
            TextLine text
                | null text -> do
                    addError etype projName modName row col
                    setMode $ LookingForError projName modName
                | otherwise -> do
                    addErrorLine text
        Finished -> undefined
-}


don'tParse p = P.option Nothing (Just <$> p) >>= \case
    Nothing -> pure ()
    Just _ -> fail "don'tParse"


log :: Parser [Error (ErrorLocation Text) Text]
log = concat <$> P.many' project <* P.endOfInput

projectHeader :: Parser (LineType Text)
projectHeader = P.many' (don'tParse projectLine *> line) *> projectLine

project :: Parser [Error (ErrorLocation Text) Text]
project = f <$> projectHeader <*>  P.many' module_
  where
    f (ProjectLine pji) modules = x pji
      where
        x :: ProjectName Text -> [Error (ErrorLocation Text) Text]
        x = fmap concat $ sequenceA modules

module_ :: Parser (ProjectName Text -> [Error (ErrorLocation Text) Text])
module_ = 
    f <$> moduleCounter <*> P.many' errorBlock
  where
    f :: LineType Text -> [ProjectName Text -> ModuleName Text -> Error (ErrorLocation Text) Text] -> ProjectName Text -> [Error (ErrorLocation Text) Text]
    f (ModuleCounter _ _ mi) blocks pji = x pji mi
      where
        x :: ProjectName Text -> ModuleName Text -> [Error (ErrorLocation Text) Text]
        x = fmap sequenceA $ sequenceA blocks
  
    
unimportantLine :: Parser (LineType Text)
unimportantLine = (don'tParse $ P.choice [P.try projectLine, P.try moduleCounter, P.try sourceLocation]) 
    *> line

blockGap :: Parser ()
blockGap = P.many' unimportantLine *> pure ()

errorBlock :: Parser (ProjectName Text -> ModuleName Text -> Error (ErrorLocation Text) Text)
errorBlock = 
    blockGap
    *> (f <$> sourceLocation <*> P.many' unimportantLine)
  where
    f :: LineType Text -> [LineType Text] -> ProjectName Text -> ModuleName Text -> Error (ErrorLocation Text) Text
    f (SourceLocation path row col IsError) lines pji mi = Error (ErrorLocation pji mi) row col $ T.unlines $ map (\(TextLine l) -> l) lines
    f (SourceLocation path row col IsWarning) lines pji mi = Warning (ErrorLocation pji mi) row col $ T.unlines $ map (\(TextLine l) -> l) lines
--errorLine = don'tParse (P.choice [P.try projectLine, P.try moduleCounter, P.try sourceLocation]) *> textLine


{-
parseLine :: ( Show s
             , IsString' s
             , MonadState (ParserState s) m
             )
          => ExceptT ParseError m ()
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
-}

newtype ParseToken = MkParseToken { unmkParseToken :: P.Result [Error (ErrorLocation Text) Text] }

startParseLog :: ParseToken
startParseLog = MkParseToken $ P.parse log ""

parseNextLogLine :: Text -> ParseToken -> ParseToken
parseNextLogLine t tok = MkParseToken $ P.feed (unmkParseToken tok) t
