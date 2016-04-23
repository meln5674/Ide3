{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
module CmdParser where

import Data.Functor.Identity

import Text.Parsec hiding (parse)
import qualified Text.Parsec as P
import Text.Parsec.Error

{-
data Cmd
    = Help
    | Open FilePath
    | Modules
    | Module String
    | Declarations
    | Imports
    | Imported
    | Exports
    | Exported
    | Visible
    | Cat String
    | Tree
    | Quit
    deriving Show
-}
parseArity0 :: Stream s m Char => String -> ParsecT s u m String
parseArity0 s = (try $ string s) >> return ""

parseArity1 :: Stream s m Char => String -> String -> ParsecT s u m String
parseArity1 s e = do
    _ <- try $ string s
    notFollowedBy eof
    _ <- string " " <?> s ++ " expects a " ++ e
    arg <- untilSpaceOrEof <?> s ++ " expects a " ++ e
    return arg

untilSpaceOrEof :: Stream s m Char => ParsecT s u m String
untilSpaceOrEof 
    =  (eof >> return [])
   <|> (space >> return [])
   <|> ((:) <$> anyChar <*> untilSpaceOrEof)

parseGarbage :: Stream s m Char => ParsecT s u m b
parseGarbage = do
    cmd <- untilSpaceOrEof
    fail $ cmd ++ ": unrecognized command"
    
lastError :: ParseError -> String
lastError p = case errorMessages p of
    [] -> "????"
    ms -> messageString $ last ms
