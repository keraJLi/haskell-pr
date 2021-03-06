
module Parser (
    Parser
  , parsePR
) where

import Prelude hiding (succ, const)
import qualified Prelude as P (succ, const)
import qualified Data.Map as Map
import Data.Functor.Identity

import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Error
import Text.Parsec.Token

import Syntax


type Parser = Parsec String NameEnv

prlang :: LanguageDef NameEnv
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

lexer :: GenTokenParser String NameEnv Identity
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

useFun :: Parser PRFun
useFun = do
  name <- identifier lexer
  env  <- getState
  case lookupFun name env of
    Nothing -> unexpected name
    Just f -> return f

comp :: Parser PRFun
comp = 
  let single = choice $ map try [ useFun, prRec, succ, const, proj ]
  in do
    f <- try (parens lexer comp) <|> single
    dot lexer
    gs <- try (parens lexer (commaSep lexer (try comp <|> single)))
          <|> fmap (: []) single
    return $ Comp f gs

prFun :: Parser PRFun
prFun = choice funs <?> "function"
  where funs = map try [ comp, useFun, prRec, succ, const, proj ]

lit :: Parser PRExpr
lit = Lit . fromInteger <$> natural lexer

useVal :: Parser PRExpr
useVal = do
  name <- identifier lexer
  env  <- getState
  case lookupVal name env of
    Nothing -> unexpected name
    Just f -> return f

app :: Parser PRExpr
app = do
  f <- prFun
  vals <- parens lexer $ commaSep lexer $ prExpr
  return $ App f vals

prExpr :: Parser PRExpr
prExpr = (try app <|> try useVal <|> lit) <?> "expression"

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

makeStatement :: [String] -> (PRExpr -> PRStat) -> Parser PRStat
makeStatement s st = do
  choice $ map (try . reserved lexer) s
  e <- prExpr
  return $ st e

showExpr = makeStatement ["sh", "show"] ShowExpr
evalExpr = makeStatement ["ev", "eval", "evaluate"] EvalExpr
traceExpr = makeStatement ["tr", "trace"] TraceExpr

showEnv :: Parser PRStat
showEnv = choice (map (try . reserved lexer) ["env", "environment"]) >> return ShowEnv

prStat :: Parser PRStat
prStat = (choice $ map try stats) <?> "statement"
  where stats = [ def, assign, showExpr, evalExpr, traceExpr, showEnv ]

parsePR :: NameEnv -> SourceName -> String -> Either ParseError (NameEnv, [PRStat])
parsePR = 
  runParser $ do
    whiteSpace lexer
    stats <- semiSep1 lexer prStat
    state <- getState
    eof
    return (state, stats)

main :: IO ()
main = do
  f <- readFile "../funs.pr"
  print $ parsePR empty "test" f
