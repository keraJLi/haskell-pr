
module Interaction (
    loadFile
  , repl
) where

import Text.Parsec (ParseError)


import Data.Maybe
import Data.Map (toList)

import Control.Monad.Trans.State
import Control.Monad.Trans.Except
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)

import Utils
import Syntax
import Parser
import Evaluator

loadFile :: String -> ExceptT ParseError IO NameEnv
loadFile f = 
  let 
    filt (ShowExpr e) = Just e
    filt _            = Nothing
  in do
    file <- liftIO $ readFile f
    (env, stats) <- except $ parsePR empty f file
    let shows = mapMaybe filt stats
    liftIO $ mapM_ print shows
    return env

handle :: NameEnv -> PRStat -> IO ()
handle _   (ShowExpr e) = print e
handle env (EvalExpr e) = 
  case runExcept (evaluate env e) of
    Left err -> putStrLn err
    Right e' -> print (last e')
handle env (TraceExpr e) =
  case runExcept (evaluate env e) of
    Left err -> putStrLn err
    Right es -> mapM_ print es
handle env ShowEnv = do
    if null (funs env) 
      then putStrLn "No functions."
      else putStrLn "Functions:" >> mapM_ pfs (toList (funs env))
    if null (vals env)
      then putStrLn "No Values."
      else putStrLn "Values:" >> mapM_ pvs (toList (vals env))
  where pfs (n, f) = putStrLn (n ++ " := " ++ show f)
        pvs (n, v) = putStrLn (n ++ " <- " ++ show v)
handle _ e = return ()

repl :: NameEnv -> IO ()
repl env = do
  ln <- getLine
  case parsePR env "<stdin>" ln of
    Left  err       -> print err >> repl env
    Right (nenv, d) -> mapM_ (handle nenv) d >> repl nenv
