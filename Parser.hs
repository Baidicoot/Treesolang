module Parser where

{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}

import Text.Parsec.Prim
import Text.Parsec.Error
import Text.Parsec.Combinator
import Text.Parsec.Pos

import qualified Data.Map as Map

import qualified Type.Simple as Kind
import Syntax
import Lexer hiding(Parser, parens)

type MyState = (Env, Binding, KindExpr)

type Parser u a = Parsec [TokenPos] u a
type Env = Kind.Env Kind.Kind Name

advance :: SourcePos -> t -> [TokenPos] -> SourcePos
advance _ _ ((_, pos):_) = pos
advance pos _ [] = pos

satisfy :: (EqToken -> Bool) -> Parser u TokenPos
satisfy f = tokenPrim show advance (\c ->
    let eqc = toEqToken c in
      if f eqc then Just c else Nothing)

tok :: EqToken -> Parser u TokenPos
tok n = satisfy (== n)

string :: [EqToken] -> Parser u [TokenPos]
string [] = return []
string (x:xs) = do
  t <- tok x
  ts <- string xs
  return (t:ts)

end :: Parser u ()
end = eof <|> do
  tok EqCloseParens
  return ()

lastTok :: Parser u TokenPos
lastTok = do
  t <- nonParensTok
  end
  return t

anyTok :: Parser u TokenPos
anyTok = nonParensTok

parens :: Parser u a -> Parser u a
parens p = do
  tok EqOpenParens
  r <- p
  tok EqCloseParens
  return r

prefixOp :: Parser () TokenPos
prefixOp = satisfy (\x -> case x of
  EqOp _ -> True
  _ -> False)

infixOp :: Parser () TokenPos
infixOp = satisfy (\x -> case x of
  EqInfix _ -> True
  _ -> False)

nonParensTok :: Parser u TokenPos
nonParensTok = satisfy $ \x -> x /= EqOpenParens && x /= EqCloseParens

parensTok :: Parser u TokenPos
parensTok = (tok EqOpenParens) <|> (tok EqCloseParens)

genData :: Env -> Env -> Token -> Maybe MyState
genData _ (Kind.Env vars) (TVar n) = case n `Map.lookup` vars of
  Just k -> Just (Kind.empty, Var n, k)
  Nothing -> Just (Kind.empty, Var n, Kind.Var n)
genData (Kind.Env env) _ (TOp n) = do
  kind <- Map.lookup n env
  return (Kind.empty, Op n, kind)
genData (Kind.Env env) _ (TInfix n) = do
  kind <- Map.lookup n env
  return (Kind.empty, Op n, kind)
genData _ _ _ = Nothing

-- actual parser
getNewState :: MyState -> MyState -> Maybe MyState
getNewState (stateEnv, stateBinding, stateKind) (_, Var n, varKind) = do
  (v, k) <- Kind.apply stateKind varKind (++"'")
  joined <- Kind.unite (Kind.Env $ Map.singleton n v) stateEnv
  return (joined, App stateBinding (Var n), k)
getNewState (stateEnv, stateBinding, stateKind) (varEnv, varBinding, varKind) = do
  (_, k) <- Kind.apply stateKind varKind (++"'")
  joined <- Kind.unite stateEnv varEnv
  return (joined, App stateBinding varBinding, k)

getSingle :: Env -> Env -> Parser () MyState
getSingle env vars = (do
  (a, pos) <- anyTok
  case genData env vars a of
    Just x -> return x
    Nothing -> fail $ "unrecognised token `" ++ show a ++ "'")
  <|> try (parens (topLevel env vars))
  <|> parens (getSingle env vars)

getInfix :: Env -> Env -> Parser () MyState
getInfix env vars = do
  (a, pos) <- infixOp
  case genData env vars a of
    Just x -> return x
    Nothing -> fail $ "unrecognised token `" ++ show a ++ "'"

getApplication :: Env -> Env -> MyState -> Parser () MyState
getApplication env vars state = (do
  fn <- getInfix env vars
  case getNewState fn state of
    Just x -> return x
    Nothing -> fail "could not match infix kinds")
  <|> (do
  appState <- getSingle env vars
  case getNewState state appState of
    Just x -> return x
    Nothing -> fail "could not match kinds")

getApplications :: Env -> Env -> MyState -> Parser () MyState
getApplications env vars state = try (do
  start <- getApplication env vars state
  r <- getApplications env vars start
  return r)
  <|> getApplication env vars state

topLevel :: Env -> Env ->  Parser () MyState
topLevel env vars = try (do
  start <- getSingle env vars
  rem <- getApplications env vars start
  return rem)
  <|> getSingle env vars

binding :: Env -> Env -> Parser () MyState
binding env vars = do
  r <- topLevel env vars
  eof <?> "eof or matched kinds"
  return r

printState :: MyState -> IO ()
printState (e, b, k) = do
  putStrLn "variables:"
  print e
  putStrLn "binding:"
  print b
  putStrLn "kind:"
  print k

testParser :: Parser () MyState -> String -> IO ()
testParser p s = case parse tokensT "<test>" s of
  Left err -> print err
  Right x -> case parse p "<test>" x of
    Left err -> print err
    Right x -> printState x

std = Kind.Env $ Map.fromList $
  [("Zero",	Kind.Base)
  ,("Succ",	Kind.Arr Kind.Base Kind.Base)
  ,("Pred",	Kind.Arr Kind.Base Kind.Base)
  ,(".",	Kind.Arr (Kind.Arr (Kind.Var "b") (Kind.Var "c")) (Kind.Arr (Kind.Arr (Kind.Var "a") (Kind.Var "b")) (Kind.Arr (Kind.Var "a") (Kind.Var "c"))))]
