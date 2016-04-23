{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
module CmdParser where

import Data.Functor.Identity

import Text.Parsec hiding (parse)
import qualified Text.Parsec as P
import Text.Parsec.Error

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

parseArity0 :: Stream s m Char => String -> ParsecT s u m String
parseArity0 s = string s >> return ""

parseArity1 :: Stream s m Char => String -> String -> ParsecT s u m String
parseArity1 s e = do
    _ <- string s
    notFollowedBy eof
    _ <- string " " <?> s ++ " expects a " ++ e
    arg <- untilSpaceOrEof <?> s ++ " expects a " ++ e
    return arg
{-
parseHelp :: Stream s m Char => ParsecT s u m Cmd
parseHelp = parseArity0 "help" Help

parseOpen:: Stream s m Char => ParsecT s u m Cmd
parseOpen = parseArity1 "open" Open "directory path"

parseModules :: Stream s m Char => ParsecT s u m Cmd
parseModules = parseArity0 "modules" Modules

parseModule :: Stream s m Char => ParsecT s u m Cmd
parseModule = parseArity1 "module" Module "module name"

parseDeclarations :: Stream s m Char => ParsecT s u m Cmd
parseDeclarations = parseArity0 "declarations" Declarations

parseImports :: Stream s m Char => ParsecT s u m Cmd
parseImports = parseArity0 "imports" Imports

parseImported :: Stream s m Char => ParsecT s u m Cmd
parseImported = parseArity0 "imported" Imported

parseExports :: Stream s m Char => ParsecT s u m Cmd
parseExports = parseArity0 "exports" Exports

parseExported :: Stream s m Char => ParsecT s u m Cmd
parseExported = parseArity0 "exported" Exported

parseVisible :: Stream s m Char => ParsecT s u m Cmd
parseVisible = parseArity0 "visible" Visible

parseCat :: Stream s m Char => ParsecT s u m Cmd
parseCat = parseArity1 "cat" Cat "smbol name"

parseTree :: Stream s m Char => ParsecT s u m Cmd
parseTree = parseArity0 "tree" Tree

parseQuit :: Stream s m Char => ParsecT s u m Cmd
parseQuit = parseArity0 "quit" Quit
-}
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
{-
parseCmd :: Stream s m Char => ParsecT s u m Cmd
parseCmd =
    try parseHelp
    <|> try parseOpen
    <|> try parseModules
    <|> try parseModule
    <|> try parseDeclarations
    <|> try parseImports
    <|> try parseImported
    <|> try parseExports
    <|> try parseExported
    <|> try parseVisible
    <|> try parseCat
    <|> try parseTree
    <|> try parseQuit
    <|> parseGarbage

parse :: Stream s Identity Char => s -> Either String Cmd
parse s = case P.parse parseCmd "" s of
    Left err -> Left $ lastError err
    Right x -> Right x

cmdList :: [String]
cmdList = 
    [ "help"
    , "open"
    , "module"
    , "modules"
    , "declarations"
    , "imports"
    , "imported"
    , "exports"
    , "exported"
    , "visible"
    , "cat"
    , "tree"
    , "quit"
    ]
-}
