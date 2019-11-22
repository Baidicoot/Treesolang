module Main where

{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-} --reee amirite

import System.Exit
import System.IO
import System.Environment (getArgs)
import Text.Parsec.Error (ParseError)

import Checker (checkRules, fromExprs, makeEnv, unravel)
import Lexer (parseExprs)
import Syntax (Parsed)
import Eval (runPrg)

doParsing :: String -> Either [ParseError] [Parsed]
doParsing str = do
  exprs <- parseExprs str
  let (defns, rules) = unravel $ fromExprs exprs
  checkRules (makeEnv defns) rules

forAllPrint :: (Show a) => [a] -> IO ()
forAllPrint [] = return ()
forAllPrint (x:xs) = do
  print x
  forAllPrint xs

main :: IO ()
main = do
  args <- getArgs
  if args == [] then do
    putStrLn "please supply a file to read."
    exitSuccess
  else do
    handle <- openFile (args !! 0) ReadMode
    contents <- hGetContents handle
    case doParsing contents of
      Left errs -> forAllPrint errs
      Right parsed -> do
        putStrLn "== SUCESS =="
        print $ runPrg parsed
