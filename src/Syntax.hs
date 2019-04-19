
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
        | Comp PR [PR]
        | Succ
        | Const Arity Int
        | Proj Arity Int
        | Rec PR PR
  deriving (Eq)

instance Show PR where
  -- Special cases
  show (App c@(Comp _ _) xs) = 
    "(" ++ show c ++ ")(" ++ (", " `intercalate` fmap show xs) ++ ")"
  show (Comp c@(Comp _ _) h) = "(" ++ show c ++ ") . " ++ show h

  -- Rest
  show (Def name p) = name ++ " = " ++ show p
  show (Use name)   = name
  show (App f xs)   = show f ++ "(" ++ (", " `intercalate` fmap show xs) ++ ")"
  show (Lit l)      = show l
  show (Comp f g)   = show f ++ " . " ++ show g
  show Succ         = "s"
  show (Const ar c) = "c[" ++ show ar ++ "," ++ show c ++ "]"
  show (Proj ar i)  = "pr[" ++ show ar ++ "," ++ show i ++ "]"
  show (Rec f g)    = "Pr{" ++ show f ++ ", " ++ show g ++ "}"
