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

main :: IO ()
main =  do
        args <- getArgs
        let path = head args
        inputAsFile <- readFile path
        let str = inputAsFile
        let a = parser str []
        print (fst (evaluateExpression (createExpression a) 0 []))