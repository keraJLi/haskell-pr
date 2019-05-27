
module SyntaxNew (
    Arity
  , Error
  , NameEnv
  , PRFun (..)
  , PRExpr (..)
  , PRStat (..)
) where

import Data.List (intercalate)

type Arity = Int
type Error = String
type NameEnv = [(String, PRExpr)]

-- Primitive recursive functions
data PRFun = Use String
           | Succ
           | Const Arity Int
           | Proj Arity Int
           | Rec PRFun PRFun
           | Comp PRFun [PRFun]
  deriving (Eq)

-- Expressions in our language. These evaluate to a certain value
data PRExpr = Lit Int
            | App PRFun [PRExpr]

-- Statements in our language. These do not evaluate to certain values
-- The Expr data will be evaluated and then printed, they do not have a value
-- in out program.
data PRStat = Def String PRFun
            | Assign String PRExpr
            | ShowExpr PRExpr
  deriving Show

instance Show PRFun where
  show (Use s)    = s
  show Succ         = "s"
  show (Const ar c) = "c[" ++ show ar ++ "," ++ show c ++ "]"
  show (Proj ar i)  = "pr[" ++ show ar ++ "," ++ show i ++ "]"
  show (Rec f g)    = "Pr{" ++ show f ++ ", " ++ show g ++ "}"
  -- This represents the right-associativity of concatenation in pr funs
  show (Comp c@(Comp _ _) h) = "(" ++ show c ++ ") . " ++ show h
  show (Comp f [g])  = show f ++ " . " ++ show g
  show (Comp f gs)   = show f ++ " . (" ++ ", " `intercalate` map show gs ++ ")"

instance Show PRExpr where
  show (Lit n)    = show n
  show (App f es) = show f ++ "(" ++ (", " `intercalate` fmap show es) ++ ")"
  -- show (App c@(Comp _ _) es) = 
  --   "(" ++ show c ++ ")(" ++ (", " `intercalate` fmap show es) ++ ")"
