module Lexer where

import qualified Type.Simple as Kind
import Syntax
import Prelude hiding(id)

import Text.Parsec
import Text.Parsec.Char

type Parser = Parsec String ()

{- The lexer parses for two root expressions:
 - a definition, consisting of '(infix) id :: kindExpr'
 - a rule, consisting of 'binding = binding'
 - where:
 - id - capitalised alphanumeric sequence, or symbols
 - kindExpr - * or alphanum seperated by () or ->
 -}

-- helper funcs
reserve :: Eq a => Parser a -> [a] -> Parser a
reserve op ls = try $ do
  n <- op
  if n `elem` ls then fail "reserved" else return n

parens :: Parser a -> Parser a
parens p = do
  char '('
  n <- p
  char ')'
  many space
  return n

-- variable
var :: Parser String
var = do
  x <- lower
  xs <- many (alphaNum <|> oneOf "_'")
  return (x:xs)

resVar :: [String] -> Parser String
resVar = reserve var

nonReserved :: Parser String
nonReserved = resVar ["infix", "data"]

-- id ('normal' part of identifier)
id :: Parser String
id = do
  x <- upper
  xs <- many (alphaNum <|> oneOf "_")
  return (x:xs)

-- op (infix part of identifier)
op :: Parser String
op = many1 $ oneOf ":!#$%&*+./<=>?@\\^|-~"

resOp :: [String] -> Parser String
resOp = reserve op

nonReservedOp :: Parser String
nonReservedOp = resOp ["~", "=>"]

reserved :: String -> Parser ()
reserved s = do
  string s
  many space
  return ()

-- token versions
tokVar :: Parser [TokenPos]
tokVar = do
  pos <- getPosition
  n <- nonReserved
  return [(TVar n, pos)]

tokId :: Parser [TokenPos]
tokId = do
  pos <- getPosition
  n <- id
  return [(TOp n, pos)]

tokOp :: Parser [TokenPos]
tokOp = do
  pos <- getPosition
  n <- nonReservedOp
  return [(TInfix n, pos)]

-- parenthesised tokens
openParens :: Parser TokenPos
openParens = do
  pos <- getPosition
  char '('
  return (TOpenParens, pos)

closeParens :: Parser TokenPos
closeParens = do
  pos <- getPosition
  char ')'
  return (TCloseParens, pos)

tokParens :: Parser [TokenPos]
tokParens = do
  open <- openParens
  toks <- tokensT
  close <- closeParens
  return (open : toks ++ [close])

tokenT :: Parser [TokenPos]
tokenT =
      tokVar
  <|> tokId
  <|> tokOp
  <|> tokParens

tokensT :: Parser [TokenPos]
tokensT = do
  toks <- tokenT `sepEndBy` (many space)
  return (concat toks)

-- kind sub-expression parser
kConcrete :: Parser KindExpr
kConcrete = do
  reserved "*"
  return Kind.Base

kVar :: Parser KindExpr
kVar = do
  n <- nonReserved
  many space
  return (Kind.Var n)

kLit :: Parser KindExpr
kLit =
      kConcrete
  <|> kVar
  <|> parens kindExpr

-- kind expression parser
kArr :: Parser KindExpr
kArr = do
  i <- kLit
  reserved "->"
  o <- kindExpr
  return (Kind.Arr i o)

kindExpr :: Parser KindExpr
kindExpr =
  try kArr
  <|> kLit

-- rule parser
rule :: Parser Rule
rule = do
  reserved "~"
  bind <- tokensT
  reserved "=>"
  ret <- tokensT
  return (Rule bind ret)

-- defn parser
dataDefn :: Parser Defn
dataDefn = do
  reserved "data"
  n <- id
  many space
  reserved "::"
  k <- kindExpr
  return (Defn n k)

infixDefn :: Parser Defn
infixDefn = do
  reserved "infix"
  n <- nonReservedOp
  many space
  reserved "::"
  k <- kindExpr
  return (DefIn n k)

defn :: Parser Defn
defn = do
  infixDefn
  <|> dataDefn

-- expr parser
ruleExpr :: Parser Expr
ruleExpr = do
  r <- rule
  return (RuleExpr r)

defnExpr :: Parser Expr
defnExpr = do
  d <- defn
  return (DefnExpr d)

expr :: Parser Expr
expr =
      ruleExpr
  <|> defnExpr

exprs :: Parser [Expr]
exprs = expr `sepBy` (many space)

parseExprs :: String -> Either [ParseError] [Expr]
parseExprs str = case parse exprs "<stdin>" str of
  Right x -> Right x
  Left x -> Left [x]
