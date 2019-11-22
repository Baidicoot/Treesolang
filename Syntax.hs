module Syntax where

import Data.List (nub)
import Text.Parsec.Pos
import Type.Simple (Kind)
import qualified Data.Map as Map

type Name = String
type KindExpr = Kind Name
type TokenPos = (Token, SourcePos)

parensIf :: Bool -> String -> String
parensIf False x = x
parensIf True x = '(':x++")"

data Token
  = TOpenParens
  | TCloseParens
  | TOp Name
  | TVar Name
  | TInfix Name
  deriving(Show)

data EqToken
  = EqOpenParens
  | EqCloseParens
  | EqOp Name
  | EqVar Name
  | EqInfix Name
  deriving(Show,Eq)

toEqToken :: TokenPos -> EqToken
toEqToken (TOp n, _) = EqOp n
toEqToken (TVar n, _) = EqVar n
toEqToken (TInfix n, _) = EqInfix n
toEqToken (TOpenParens, _) = EqOpenParens
toEqToken (TCloseParens, _) = EqCloseParens

data Expr
  = RuleExpr Rule
  | DefnExpr Defn
  deriving(Show)

data Binding
  = App Binding Binding
  | Var Name
  | Op Name
  deriving(Eq)

allElem :: (Eq a) => [a] -> [a] -> Bool
allElem [] _ = False
allElem _ [] = True
allElem es (x:xs) = (x `elem` es) && (allElem es xs)

showBinding :: Bool -> Binding -> String
showBinding _ (Var n) = n
showBinding _ (Op n) = parensIf (allElem ":!#$%&*+./<=>?@\\^|-~" n) n
showBinding b (App x y) = parensIf b $ (showBinding False x) ++ " " ++ (showBinding True y)

instance Show Binding where
  show = showBinding False

data Defn
  = Defn Name KindExpr
  | DefIn Name KindExpr
  deriving(Show)

data Rule
  = Rule [TokenPos] [TokenPos]
  deriving(Show)

data Parsed
  = PRule Binding Binding

instance Show Parsed where
  show (PRule i o) = "~ " ++ (show i) ++ " => " ++ (show o)

isVar :: TokenPos -> Bool
isVar (TVar _, _) = True
isVar _ = False

bracketify :: String -> String
bracketify s = '(' : s ++ ")"
