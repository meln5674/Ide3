{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module CmdParser where

import Data.Monoid

import Data.String (IsString(..))

import Data.Text (Text)
import qualified Data.Text as T

import Control.Monad

import Text.Parsec hiding (parse)
import Text.Parsec.Error

-- | Parse a command taking no arguments
parseArity0 :: Stream s m Char => Text -> ParsecT s u m Text
parseArity0 s = try (string $ T.unpack s) >> return ""

-- | Parse a command taking a single argument
parseArity1 :: Stream s m Char => Text -> Text -> ParsecT s u m Text
parseArity1 s e = do
    _ <- try $ string $ T.unpack s
    notFollowedBy eof
    _ <- string " " <?> (T.unpack $ s <> " expects a " <> e)
    --arg <- untilSpaceOrEof <?> s ++ " expects a " ++ e
    T.pack <$> manyTill anyChar eof

quotedString :: Stream s m Char => ParsecT s u m Text
quotedString = do
    _ <- char '\"'
    liftM T.pack $ liftM concat
        $ manyTill 
          (many (noneOf ['\\', '\"']) 
            <|> string "\\\\" 
            <|> string "\\\""
          )
        $ string "\""

-- | Parse a command taking any number of arguments
parseArityN' :: Stream s m Char => ParsecT s u m [Text]
parseArityN' = do
    spaces
    args <- sepBy (try quotedString <|> (T.pack <$> many1 (notFollowedBy spaces >> anyChar))) spaces
    return args

parseArityN :: Text -> Either String [Text]
parseArityN str = case runParser parseArityN' () "" str of
    Left err -> Left $ show err
    Right x -> Right x

-- | Read characters until a space or the end of input
untilSpaceOrEof :: Stream s m Char => ParsecT s u m String
untilSpaceOrEof 
    =  (eof >> return [])
   <|> (space >> return [])
   <|> ((:) <$> anyChar <*> untilSpaceOrEof)

-- | Parse the first word of input and create a failure message using it
parseGarbage :: Stream s m Char => ParsecT s u m b
parseGarbage = do
    cmd <- untilSpaceOrEof
    fail $ cmd ++ ": unrecognized command"
    
-- | Get the last error message from a parse error
lastError :: ParseError -> Text
lastError p = case errorMessages p of
    [] -> "????"
    ms -> T.pack $ messageString $ last ms
