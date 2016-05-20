{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
module CmdParser where

import Data.Functor.Identity

import Text.Parsec hiding (parse)
import qualified Text.Parsec as P
import Text.Parsec.Error

-- | Parse a command taking no arguments
parseArity0 :: Stream s m Char => String -> ParsecT s u m String
parseArity0 s = (try $ string s) >> return ""

-- | Parse a command taking a single argument
parseArity1 :: Stream s m Char => String -> String -> ParsecT s u m String
parseArity1 s e = do
    _ <- try $ string s
    notFollowedBy eof
    _ <- string " " <?> s ++ " expects a " ++ e
    arg <- untilSpaceOrEof <?> s ++ " expects a " ++ e
    return arg

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
lastError :: ParseError -> String
lastError p = case errorMessages p of
    [] -> "????"
    ms -> messageString $ last ms