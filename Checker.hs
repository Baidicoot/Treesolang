module Checker where

{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}

import Text.Parsec.Error
import Text.Parsec (parse)
import qualified Data.Map as Map

import Parser (binding, Env, std)
import Syntax
import qualified Type.Simple as Kind

data Prg = Prg [Defn] [Rule]
unravel :: Prg -> ([Defn], [Rule])
unravel (Prg a b) = (a, b)

addTo :: Expr -> Prg -> Prg
addTo (DefnExpr x) (Prg ds rs) = Prg (x:ds) rs
addTo (RuleExpr x) (Prg ds rs) = Prg ds (x:rs)

fromExprs :: [Expr] -> Prg
fromExprs [] = Prg [] []
fromExprs (x:xs) = addTo x (fromExprs xs)

makeEnv :: [Defn] -> Env
makeEnv [] = Kind.Env Map.empty
makeEnv ((Defn n k):ds) = Kind.Env $ Map.insert n k (Kind.underlying $ makeEnv ds)
makeEnv ((DefIn n k):ds) = Kind.Env $ Map.insert n k (Kind.underlying $ makeEnv ds)

checkRule :: Env -> Rule -> Either ParseError Parsed
checkRule env (Rule m r) = do
  (mv, mb, mk) <- parse (binding env Kind.empty) "<checker>" m
  (rv, rb, rk) <- parse (binding env mv) "<checker>" r
  case Kind.bind mk rk of
    Just _ -> return $ PRule mb rb
    Nothing -> Left $ newErrorMessage (matchError mk rk) headPos
  where
    (_, headPos) = head m
    matchError mk rk = Message $ "Kinds `" ++ show mk ++ "' and `" ++ show rk ++ "' do not match."

appendErrorList :: Either a b -> Either [a] [b] -> Either [a] [b]
appendErrorList (Left x) (Left xs) = Left (x:xs)
appendErrorList _ (Left xs) = Left xs
appendErrorList (Left x) _ = Left [x]
appendErrorList (Right x) (Right xs) = Right (x:xs)

checkRules :: Env -> [Rule] -> Either [ParseError] [Parsed]
checkRules _ [] = Right []
checkRules env (x:xs) = appendErrorList (checkRule env x) (checkRules env xs)
