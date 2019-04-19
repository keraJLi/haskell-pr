module CommandLine (
    Command (..)
  , usage
  , oops
  , parseCommand
) where

data Command = Repl
             | Load String
             | Exec String
             | Help

commands :: [String]
commands = ["--help", "-h", "--execute", "-e"]

oops :: String
oops = "Oops, that command was not recognized. Try --help"

usage :: String
usage = unlines [
    "PR Usage:"
  , "primrec [--execute <file>, <file>, --help]"
  , "  file           - Path of the file to be loaded into REPL, "
  , "                   REPL is not started if --execute option is set"
  , "  --execute -e   - Only execute the file (no REPL launched)"
  , "  --help -h      - Show this message"
  ]

parseCommand :: [String] -> Maybe Command
parseCommand []  = Just Repl
parseCommand [f]
  | f `elem` ["--help", "-h"] = Just Help
  | f `notElem` commands      = Just (Load f)
  | otherwise                 = Nothing
parseCommand [e, f]
  | e `elem` ["--execute", "-e"] = Just (Exec f)
  | otherwise                    = Nothing
parseCommand _ = Nothing
