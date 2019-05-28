module Evaluator (
    checkArity
  , reduce
  , evaluate
) where 

import Control.Monad (unless)
import Control.Monad.Trans.Except
import Control.Monad.Trans.Writer
import Control.Monad.Trans.Class (lift)

import Utils
import Syntax

checkArity :: NameEnv -> PRFun -> Except Error Int
checkArity _ Succ = return 1
checkArity _ (Const ar c) 
  | ar < 0 = throwE $ "Arity was smaller than 0 in " ++ show (Const ar c) 
  | otherwise = return ar
checkArity _ pr@(Proj ar p) 
  | ar < p = throwE $ "Arity was smaller than projection index in " ++ show pr
  | ar < 1 = throwE $ "Arity was smaller than 1 in " ++ show pr
  | p  < 0 = throwE $ "Projection index was smaller than 0 in " ++ show pr
  | otherwise = return ar
checkArity e (Comp f g) = do
  arf <- checkArity e f
  arg <- mapM (checkArity e) g
  unless (allEq arg) $
    throwE $ "The functions in " ++ show g ++ " have different arities!"
  unless (arf == length arg) $
    throwE $ show f ++ " does not have and arity of one!"
  return (head arg)
checkArity e (Rec f g) = do
  arf <- checkArity e f
  arg <- checkArity e g
  if arf + 1 == arg - 1
    then return (arf + 1)
    else throwE $ sf ++ " and " ++ sg ++ " do not have arities k-1 and k+1!"
  where (sf, sg) = (show f, show g)

reduce :: NameEnv -> PRExpr -> Except Error PRExpr
reduce _ (App (Const _ c) _)   = return $ Lit c
reduce _ (App (Proj _ p) xs)   = return $ xs !! (p-1)
reduce _ (App Succ [Lit x])    = return $ Lit (x + 1)
reduce e (App Succ [x])        = reduce e x >>= \ n -> return $ App Succ [n]
reduce _ (App (Comp f gs) xs)  = return $ App f [App g xs | g <- gs]
reduce e (App (Rec f g) xs)    =
  case last xs of
    Lit 0 -> return $ App f (init xs)
    Lit y -> return $ App g (init xs ++ [Lit (y-1)] 
                            ++ [App (Rec f g) (init xs ++ [Lit (y-1)])])
    _ -> do
      p <- reduce e (last xs)
      return $ App (Rec f g) (init xs ++ [p])
reduce _ p = throwE $ "Did not know what to do while reducing " ++ show p

evaluate :: NameEnv -> PRExpr ->  Except Error [PRExpr]
evaluate env x = snd <$> runWriterT (wr x)
  where wr :: PRExpr -> WriterT [PRExpr] (Except Error) Int
        wr p = do
          tell [p]
          red <- lift $ reduce env p
          case red of
            Lit l -> tell [red] >> return l
            p'    -> wr p'
