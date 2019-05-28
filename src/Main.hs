module Main (
    main
) where

import System.Environment
import Control.Monad.Trans.Except (runExceptT)

import CommandLine
import Interaction
import Syntax (empty)

execCmd :: Command -> IO ()
execCmd Help = putStrLn usage
execCmd Repl = repl empty
execCmd (Exec f) = do
  lns <- readFile f
  _   <- runExceptT $ loadFile lns
  return ()
execCmd (Load f) = do
  menv <- runExceptT $ loadFile f
  case menv of
    Left err  -> print err
    Right env -> putStrLn (f ++ " loaded.") >> repl env

main :: IO ()
main = do
  args <- getArgs
  case parseCommand args of
    Nothing  -> putStrLn oops
    Just cmd -> execCmd cmd
