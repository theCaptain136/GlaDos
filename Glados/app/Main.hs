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


-- main :: IO ()
-- main =  do  someFunc
--             testFunc1
--             testFunc2

main :: IO ()
main = print (fst (evaluateExpression (SymbolExpression "a" [(Symbol "" (Value (ValueInt 3) ""))]) 0 [[(Symbol "a" (Lambda [(Symbol "b" (Value (ValueError (Error 1)) ""))] [(Define "c" (Value (ValueInt 2) "")), (Plus (Value (ValueError (Error 1)) "c") (Value (ValueError (Error 1)) "b"))]))]]))
