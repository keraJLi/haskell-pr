
module Syntax (
    Arity
  , Error
  , NameEnv
  , PR (..)
) where

import Data.List (intercalate)

type Arity = Int
type Error = String
type NameEnv = [(String, PR)]

data PR = Def String PR
        | Use String
        | App PR [PR]
        | Lit Int
        | Conc PR PR
        | Succ
        | Const Arity Int
        | Proj Arity Int
        | Rec PR PR
  deriving (Eq)

instance Show PR where
  show (Def name p) = name ++ " = " ++ show p
  show (Use name)   = name
  show (App c@(Conc _ _) xs) = "(" ++ show c ++ ")(" ++ (", " `intercalate` fmap show xs) ++ ")"
  show (App f xs)   = show f ++ "(" ++ (", " `intercalate` fmap show xs) ++ ")"
  show (Lit l)      = show l
  show (Conc f g)   = show f ++ " . " ++ show g
  show Succ         = "s"
  show (Const ar c) = "c[" ++ show ar ++ "," ++ show c ++ "]"
  show (Proj ar i)  = "pr[" ++ show ar ++ "," ++ show i ++ "]"
  show (Rec f g)    = "Pr{" ++ show f ++ ", " ++ show g ++ "}"