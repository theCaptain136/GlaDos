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