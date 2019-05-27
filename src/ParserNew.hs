
module ParserNew (

) where

import Prelude hiding (succ, const)
import qualified Prelude as P (succ, const)
import qualified Data.Map as Map
import Data.Functor.Identity

import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Error
import Text.Parsec.Token

import SyntaxNew

data ParseState = ParseState {
    funs :: Map.Map String PRFun
  , vals :: Map.Map String PRExpr
  }
  deriving (Show)

lookupFun :: String -> ParseState -> Maybe PRFun
lookupFun f s = Map.lookup f (funs s)

lookupVal :: String -> ParseState -> Maybe PRExpr
lookupVal f s = Map.lookup f (vals s)

putFun :: String -> PRFun -> ParseState -> ParseState
putFun name fun state = state { funs = Map.insert name fun (funs state) }

putVal :: String -> PRExpr -> ParseState -> ParseState
putVal name val state = state { vals = Map.insert name val (vals state) }

empty :: ParseState
empty = ParseState { funs = Map.empty, vals = Map.empty}

type Parser = Parsec String ParseState

prlang :: LanguageDef ParseState
prlang = LanguageDef {
    commentStart = "{-"
  , commentEnd = "-}"
  , commentLine = "--"
  , nestedComments = True
  , identStart = letter <|> char '-'
  , identLetter = letter <|> oneOf ['0'..'9']
  , opStart = char '.'
  , opLetter = char '.'
  , reservedNames = [":=", "<-", "show"]
  , reservedOpNames = ["c", "s", "pr", "Pr"]
  , caseSensitive = True
}

lexer :: GenTokenParser String ParseState Identity
lexer = makeTokenParser prlang

succ :: Parser PRFun
succ = reservedOp lexer "s" >> return Succ

cpHelper :: Parser (Int, Int)
cpHelper = brackets lexer $ do
  ar <- natural lexer
  comma lexer
  num <- natural lexer
  return (fromInteger ar, fromInteger num)

const :: Parser PRFun
const = do
  reservedOp lexer "c"
  (ar, num) <- cpHelper
  return (Const ar num)

proj :: Parser PRFun
proj = do
  reservedOp lexer "pr"
  (ar, num) <- cpHelper
  return (Proj ar num)

prRec :: Parser PRFun
prRec = do
  reservedOp lexer "Pr"
  (f1, f2) <- braces lexer $ do
    f1 <- prFun
    comma lexer
    f2 <- prFun
    return (f1, f2)
  return (Rec f1 f2)

use :: Parser PRFun
use = do
  name <- identifier lexer
  env  <- getState
  case lookupFun name env of
    Nothing -> unexpected name
    Just f -> return f

comp :: Parser PRFun
comp = 
  let single = choice $ map try [ use, prRec, succ, const, proj ]
  in do
    f <- try (parens lexer comp) <|> single
    dot lexer
    gs <- try (parens lexer (commaSep lexer (try comp <|> single)))
          <|> fmap (: []) single
    return $ Comp f gs

prFun :: Parser PRFun
prFun = choice funs <?> "function"
  where funs = map try [ comp, use, prRec, succ, const, proj ]

lit :: Parser PRExpr
lit = Lit . fromInteger <$> natural lexer

app :: Parser PRExpr
app = do
  f <- prFun
  vals <- parens lexer $ commaSep lexer $ prExpr
  return $ App f vals

prExpr :: Parser PRExpr
prExpr = try app <|> lit <?> "statement"

def :: Parser PRStat
def = do
  name <- identifier lexer
  reserved lexer ":="
  f <- prFun
  modifyState (putFun name f)
  return $ Def name f

assign :: Parser PRStat
assign = do
  name <- identifier lexer
  reserved lexer "<-"
  val <- prExpr
  modifyState (putVal name val)
  return $ Assign name val

showExpr :: Parser PRStat
showExpr = do
  reserved lexer "show"
  e <- prExpr
  return $ ShowExpr e

prStat :: Parser PRStat
prStat = choice $ map try [ def, assign, showExpr ]

parsePR :: SourceName -> String -> Either ParseError (ParseState, [PRStat])
parsePR = runParser parser empty
  where parser = do
          whiteSpace lexer
          stats <- semiSep1 lexer prStat
          state <- getState
          eof
          return (state, stats)

parser = do
  whiteSpace lexer
  stats <- prStat `sepBy` reserved lexer ";"
  state <- getState
  eof
  return (state, stats)

main :: IO ()
main = do
  f <- readFile "../funs.pr"
  print $ parsePR "test" f
