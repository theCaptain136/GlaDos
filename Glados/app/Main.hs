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

main :: IO ()
main = do
  print (fst (evaluateExpression (SymbolExpression "add" [(Symbol "" (Value (ValueInt 2) "")), (Symbol "" (SymbolExpression "foo" []))]) 0 [[(Symbol "add" (Lambda [(Symbol "a" (Value (ValueError (Error 1)) "")), (Symbol "b" (Value (ValueError (Error 1)) ""))] [(Plus (Value (ValueError (Error 1)) "a") (Value (ValueError (Error 1)) "b"))])), (Symbol "foo" (Value (ValueInt 3) ""))]]))
  -- args <- getArgs
  -- if args == []
  --   then
  --     inputLoop [[]]
  --   else do
  --     let path = head args
  --     inputAsFile <- readFile path
  --     let str = inputAsFile
  --     let a = parser str []
  --     loop a [[]] 0