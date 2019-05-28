
module Syntax (
    PRFun (..)
  , PRExpr (..)
  , PRStat (..)
  , Arity
  , Error
  , NameEnv
  , empty
  , lookupFun
  , lookupVal
  , putFun
  , putVal
) where

import Data.List (intercalate)
import qualified Data.Map as Map

type Arity = Int
type Error = String

data NameEnv = NameEnv {
    funs :: Map.Map String PRFun
  , vals :: Map.Map String PRExpr
  }
  deriving (Show)

lookupFun :: String -> NameEnv -> Maybe PRFun
lookupFun f s = Map.lookup f (funs s)

lookupVal :: String -> NameEnv -> Maybe PRExpr
lookupVal f s = Map.lookup f (vals s)

putFun :: String -> PRFun -> NameEnv -> NameEnv
putFun name fun state = state { funs = Map.insert name fun (funs state) }

putVal :: String -> PRExpr -> NameEnv -> NameEnv
putVal name val state = state { vals = Map.insert name val (vals state) }

empty :: NameEnv
empty = NameEnv { funs = Map.empty, vals = Map.empty}

-- Primitive recursive functions
data PRFun = Succ
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
