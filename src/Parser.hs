
module Parser (
    PR (..)
  , Arity
  , Error
  , parse
  , expr, def
) where

import Control.Monad (guard)
import Prelude hiding (succ, const)

import Text.Read (readMaybe)
import Control.Monad.Trans.Except
import Text.ParserCombinators.ReadP

import Syntax
import Utils (removeSuffix)


reserved :: String -> ReadP ()
reserved s = do
  skipSpaces
  _ <- string s
  skipSpaces
  return ()

betweenReserved :: String -> String -> ReadP a -> ReadP a
betweenReserved open close r = 
  do
    reserved open
    s <- r
    reserved close
    return s

number :: ReadP Int
number = do
  mn <- many1 (satisfy (`elem` ['0'..'9']))
  case (readMaybe mn :: Maybe Int) of
    Nothing -> pfail
    Just n -> return n

succ :: ReadP PR
succ = char 's' >> return Succ

const :: ReadP PR
const = do
  reserved "c"
  betweenReserved "[" "]" $ do
    ar <- number
    reserved ","
    num <- number
    return (Const ar num)

proj :: ReadP PR
proj = do
  reserved "pr"
  betweenReserved "[" "]" $ do
    ar <- number
    reserved ","
    num <- number
    return (Proj ar num)

rec :: ReadP PR
rec = do
  reserved "Pr"
  betweenReserved "{" "}" $ do
    f <- expr
    reserved ","
    g <- expr
    return (Rec f g)

def :: ReadP PR
def = do
  name <- munch1 (`elem` ['a'..'z'])
  reserved "="
  e <- expr
  return (Def name e)

use :: ReadP PR
use = fmap Use $ do
  name <- munch1 (`elem` ['a'..'z'])
  guard (name `notElem` ["pr", "c", "s"])
  return name

func :: ReadP PR
func = use <++ choice [succ, const, proj, rec]

comp :: ReadP PR
comp = do
  f <- func +++ betweenReserved "(" ")" comp
  reserved "."
  gs <- fmap (: []) func
        +++ betweenReserved "(" ")" ((func +++ comp) `sepBy1` reserved ",")
  return $ Comp f gs

app :: ReadP [PR]
app = betweenReserved "(" ")" ((Lit <$> number) `sepBy` reserved ",")

expr' :: ReadP PR
expr' = do
  fun <- def +++ func +++ comp
  a <- option Nothing (Just <$> app)
  case a of
    Nothing -> return fun
    Just x  -> return (App fun x)

expr :: ReadP PR
expr = betweenReserved "(" ")" expr' +++ expr'

parse :: String -> Except Error PR
parse s = 
  case readP_to_S (skipSpaces >> expr) s of
    [] -> throwE "Syntax error while parsing expression!"
    xs -> case last xs of
      (p, [])   -> return p
      (_, rest) -> throwE $ "Syntax error: " 
                         ++ removeSuffix rest s ++ "<here>" ++ rest