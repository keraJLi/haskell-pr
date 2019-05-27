
module Interaction (
    loadFile
  , repl
) where

import Control.Monad.Trans.State
import Control.Monad.Trans.Except
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)

import Utils
import Syntax
import Parser
import Evaluator


parseFile :: String -> ExceptT Error IO [PR]
parseFile lns = do
  let ps = split ';' lns
  case fmap parse ps of
    []  -> return []
    pps -> liftExceptT $ sequence (scanl1 (>>) pps)

loadFile :: String -> ExceptT Error IO NameEnv
loadFile f = 
  let deal :: PR -> StateT NameEnv (ExceptT Error IO) ()
      deal (Def name fun) = modify ((name, fun) :)
      deal p = do
        env <- get
        let ev = runExcept $ evaluate env p
        case ev of
          Right p' -> liftIO $ putStrLn $ show p ++ " = " ++ show p'
          Left err -> liftIO (putStrLn err) >> lift (throwE err)
  in do
    ps  <- parseFile f
    let envState = foldr1 (>>) (deal <$> ps)
    env <- execStateT envState []
    _   <- liftExceptT (mapM (checkArity env) ps)
    return env

repl :: NameEnv -> IO ()
repl env = do
  ln <- getLine
  nenv <- case runExcept (parse ln) of
    Left  err                -> print err >> return env
    Right (d@(Def name fun)) -> print d >> return ((name, fun) : env)
    Right p ->
      case runExcept (evaluate env p) of
        Right prs -> mapM_ print prs >> return env
        Left err -> putStrLn err >> return env
  repl nenv
