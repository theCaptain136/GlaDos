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
main = print (fst (evaluateExpression (Plus (Value (ValueError (Error 1)) "first") (Value (ValueInt 4) "second")) 0 [[(Symbol "first" (Value (ValueInt 3) ""))]]))
