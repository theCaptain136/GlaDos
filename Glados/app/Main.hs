--
-- EPITECH PROJECT, 2023
-- B-FUN-500-BER-5-2-glados-pablo-elias.herrmann
-- File description:
-- Main
--

module Main (main) where

import Lib
import Translator
import Parser
import AST
import Evaluate
import System.Environment
import System.IO
import Loop

getRep :: Symbol -> Expression
getRep symbol = rep symbol

getSyName :: Symbol -> String
getSyName symbol = name symbol

getItemAtIndex :: [Symbol] -> Int -> Symbol
getItemAtIndex xs idx
  | idx >= 0 && idx < length xs = (xs !! idx)
  | otherwise = Symbol "error" (Value(ValueError (Error 90)) "")

main :: IO ()
main = do
  print (fst (evaluateExpression (SymbolExpression "add" [(Symbol "" (Value (ValueInt 5) "")), (Symbol "" (Plus (Value (ValueInt 1) "") (Value (ValueInt 1) "")))]) 0 [[Symbol "add" (Lambda [(Symbol "a" (Value (ValueError (Error 1)) "b")), (Symbol "b" (Value (ValueError (Error 1)) "b"))] [(Plus (Value (ValueError (Error 1)) "a") (Value (ValueError (Error 1)) "b"))])]]))
  print (fst (evaluateExpression (SymbolExpression "add" [(Symbol "" (Value (ValueInt 2) "")), (Symbol "" (SymbolExpression "foo" []))]) 0 [[(Symbol "add" (Lambda [(Symbol "a" (Value (ValueError (Error 1)) "")), (Symbol "b" (Value (ValueError (Error 1)) ""))] [(Plus (Value (ValueError (Error 1)) "a") (Value (ValueError (Error 1)) "b"))])), (Symbol "foo" (Value (ValueInt 3) ""))]]))
  args <- getArgs
  if args == []
    then
      inputLoop [[]]
    else do
      let path = head args
      inputAsFile <- readFile path
      let str = inputAsFile
      let a = parser str []
      loop a [[]] 0

syPrint :: [Symbol] -> IO()
syPrint [] = putStr ""
syPrint (x:xs) = do
  print (getSyName x)
  syPrint xs