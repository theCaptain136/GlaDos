--
-- EPITECH PROJECT, 2023
-- B-FUN-500-BER-5-2-glados-pablo-elias.herrmann
-- File description:
-- Translator
--


module Translator
    ( testFunc2,
    translator,
    createExpression,
    getNext,
    ) where

import Data.Text.IO (getLine)
import AST

testFunc2 :: IO()
testFunc2 = putStrLn("hello from Translator")

isBool :: String -> Bool
isBool "#t" = True
isBool "#f" = True
isBool _ = False

toBool :: String -> Bool
toBool "#t" = True
toBool "#f" = False

onlyNumbers :: String -> Bool
onlyNumbers [] = True
onlyNumbers ('-':xs) = (length xs > 0) && onlyNumbers xs
onlyNumbers (x:xs) = isDigit x && onlyNumbers xs
    where isDigit c = c >= '0' && c <= '9'

stringToInt :: String -> Int
stringToInt str = read str :: Int

createExpression :: [String] -> Expression
createExpression scm = translator scm 0 1

getNext :: [String] -> [String] -> Int -> ([String], [String])
getNext []  save c = (save, [])
getNext a b c | b /= [] && c == 0 = (b, a)
getNext ("(":xs) save c = getNext xs (save ++ ["("]) (c+1)
getNext (")":xs) save c = getNext xs (save ++ [")"]) (c-1)
getNext (x:xs) save c = getNext xs (save ++ [x]) c

translator :: [String] -> Int -> Int -> Expression
translator [] depth x | depth /= 0 = (Value (ValueError (Error 86)) "error")
translator ((h:hs):xs) depth x  | depth <= 0 && h == ')' = (Value (ValueError (Error 86)) "error")
                                | h == '(' && x == 1 = translator xs (depth+1) 0
                                | h == ')' = translator xs (depth-1) 1

translator (x:xs) depth 1   | (onlyNumbers x) == True = Value (ValueInt (stringToInt x)) ""
                            | (isBool x) == True = Value (ValueBool (toBool x)) ""

translator ("+":a:b:xs) depth 0 = let   n = (getNext (a:b:xs) [] 0)
                                        n2 = ( getNext (snd n) [] 0)
                                    in
                                        (Plus (translator (fst n) depth 1) (translator (fst n2) depth 1)) -- Plus function

translator ("-":a:b:xs) depth 0 = let   n = (getNext (a:b:xs) [] 0)
                                        n2 = ( getNext (snd n) [] 0)
                                    in
                                        (Minus (translator (fst n) depth 1) (translator (fst n2) depth 1)) -- Plus function

translator ("*":a:b:xs) depth 0 = let   n = (getNext (a:b:xs) [] 0)
                                        n2 = ( getNext (snd n) [] 0)
                                    in
                                        (Times (translator (fst n) depth 1) (translator (fst n2) depth 1)) -- Plus function

translator ("div":a:b:xs) depth 0 = let n = (getNext (a:b:xs) [] 0)
                                        n2 = ( getNext (snd n) [] 0)
                                    in
                                        (Divided (translator (fst n) depth 1) (translator (fst n2) depth 1)) -- Plus function

translator ("mod":a:b:xs) depth 0 = let n = (getNext (a:b:xs) [] 0)
                                        n2 = ( getNext (snd n) [] 0)
                                    in
                                        (Modulo (translator (fst n) depth 1) (translator (fst n2) depth 1)) -- Plus function

translator ("eq?":a:b:xs) depth 0 = let n = (getNext (a:b:xs) [] 0)
                                        n2 = ( getNext (snd n) [] 0)
                                    in
                                        (Equal (translator (fst n) depth 1) (translator (fst n2) depth 1)) -- Plus function

translator ("<":a:b:xs) depth 0 = let   n = (getNext (a:b:xs) [] 0)
                                        n2 = ( getNext (snd n) [] 0)
                                    in
                                        (Smaller (translator (fst n) depth 1) (translator (fst n2) depth 1)) -- Plus function


translator ("if":a:b:xs) depth 0 = let  n = (getNext (a:b:xs) [] 0)
                                        n2 = ( getNext (snd n) [] 0)
                                        n3 = ( getNext (snd n2) [] 0)
                                    in
                                        (Condition (translator (fst n) depth 1) (translator (fst n2) depth 1) (translator (fst n3) depth 1)) -- Plus function

translator ("define":a:b:xs) depth 0 = let  n = (getNext (a:b:xs) [] 0)
                                            n2 = ( getNext (snd n) [] 0)
                                    in
                                        (Define (head (fst n)) (translator (fst n2) depth 1)) -- Plus function

translator (x:xs) depth e = (Value (ValueError (Error 1)) x)
translator _ _ _ = (Value (ValueError (Error 900)) "error")