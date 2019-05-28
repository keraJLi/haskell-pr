
module Interaction (
    loadFile
  , repl
) where

import Text.Parsec (ParseError)

import Data.Maybe

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

repl :: NameEnv -> IO ()
repl env = do
  ln <- getLine
  case parsePR env "repl" ln of
    Left  err       -> print err >> repl env
    Right (nenv, d) -> print d >> repl nenv
