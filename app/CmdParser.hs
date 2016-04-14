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
    | Visible
    | Cat String
    | Quit
    deriving Show

parseArity0 s v = string s >> return v
parseArity1 s c e = do
    string s
    notFollowedBy eof
    string " " <?> s ++ " expects a " ++ e
    arg <- untilSpaceOrEof <?> s ++ " expects a " ++ e
    return $ c arg

parseHelp = parseArity0 "help" Help
parseOpen = parseArity1 "open" Open "directory path"
parseModules = parseArity0 "modules" Modules
parseModule = parseArity1 "module" Module "module name"
parseDeclarations = parseArity0 "declarations" Declarations
parseImports = parseArity0 "imports" Imports
parseImported = parseArity0 "imported" Imported
parseExports = parseArity0 "exports" Exports
parseExported = parseArity0 "exported" Exported
parseVisible = parseArity0 "visible" Visible
parseCat = parseArity1 "cat" Cat "smbol name"
parseQuit = parseArity0 "quit" Quit

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
    <|> (try parseVisible)
    <|> (try parseCat)
    <|> (try parseQuit)
    <|> parseGarbage


parse s = case P.parse parseCmd "" s of
    Left err -> Left $ lastError err
    Right x -> Right x


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
    , "quit"
    ]
