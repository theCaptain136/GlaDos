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
import System.Exit (exitSuccess)

getRep :: Symbol -> Expression
getRep symbol = rep symbol

getSyName :: Symbol -> String
getSyName symbol = name symbol

main :: IO ()
main = do
  args <- getArgs
  if args == []
    then
      inputLoop [[]]
    else do
      let path = head args
      inputAsFile <- readFile path
      let str = inputAsFile
      let a = (convert (parser str []))
      loop a [[]] 0
      exitSuccess

syPrint :: [Symbol] -> IO()
syPrint [] = putStr ""
syPrint (x:xs) = do
  print (getSyName x)
  print (fst (evaluateExpression (getRep x) 0 [[]]))
  syPrint xs