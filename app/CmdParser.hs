{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
module CmdParser where

import Text.Parsec hiding (parse)
import qualified Text.Parsec as P
import Text.Parsec.Char
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
    | Quit
    deriving Show

parseHelp = string "help" >> return Help

parseOpen = do
    string "open"
    notFollowedBy eof
    string " " <?> "open expects a directory path"
    path <- many1 anyChar <?> "open expects a directory path"
    return $ Open path

parseModules = string "modules" >> return Modules

parseModule = do
    string "module"
    notFollowedBy eof
    string " " <?> "module expects a module name"
    name <- many1 anyChar <?> "module expects a module name"
    return $ Module name

parseDeclarations = string "declarations" >> return Declarations

parseImports = string "imports" >> return Imports
parseImported = string "imported" >> return Imported

parseExports = string "exports" >> return Exports
parseExported = string "exported" >> return Exported

parseQuit = string "quit" >> return Quit

untilSpaceOrEof 
    =  (eof >> return [])
   <|> (space >> return [])
   <|> ((:) <$> anyChar <*> untilSpaceOrEof)

parseGarbage = do
    cmd <- untilSpaceOrEof
    fail $ cmd ++ ": unrecognized command"
    
lastError p = case errorMessages p of
    [] -> "????"
    ms -> messageString $ last ms

parseCmd =
    (try parseHelp)
    <|> (try parseOpen)
    <|> (try parseModules)
    <|> (try parseModule)
    <|> (try parseDeclarations)
    <|> (try parseImports)
    <|> (try parseImported)
    <|> (try parseExports)
    <|> (try parseExported)
    <|> (try parseQuit)
    <|> parseGarbage


parse s = case P.parse parseCmd "" s of
    Left err -> Left $ lastError err
    Right x -> Right x
