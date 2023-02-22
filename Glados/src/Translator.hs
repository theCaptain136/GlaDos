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
    getName,
    strToSybol,
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
                                | h == '(' = translator xs (depth+1) 0 -- maybe need to add x == 0 in condition
                                | h == ')' = translator xs (depth-1) 1
                                -- | h == '(' =    let res = getNext ("(":xs) [] 0
                                --                     in
                                --                         translator (fst res) (depth+1) 0



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
                                        (defineCall (fst n) (fst n2) 0)
                                        -- (Define (fst name) (translator (fst n2) depth 1)) -- Plus function

translator (x:xs) depth 0 = SymbolExpression x (strToSybol xs 0)    -- custom functions

translator (x:xs) depth 1   | (onlyNumbers x) == True = Value (ValueInt (stringToInt x)) "" -- only const values + variables
                            | (isBool x) == True = Value (ValueBool (toBool x)) ""
                            | otherwise = Value (ValueError (Error 1)) x


-- put into lamda function
-- translator (x:xs) depth 2

translator _ _ _ = (Value (ValueError (Error 900)) "error")

strToSybol :: [String] -> Int -> [Symbol]
strToSybol [] c = []
strToSybol ("(":rs) 0 = strToSybol rs 1
strToSybol (")":[]) _ = []
strToSybol (h:rs) c | onlyNumbers h = ((Symbol "" (Value (ValueInt (stringToInt h)) "")):strToSybol rs (c+1))
                    | isBool h = ((Symbol "" (Value (ValueBool (toBool h)) "")):strToSybol rs (c+1))
                    | h /= "(" && h /= ")" = (Symbol h (Value (ValueError (Error 1)) h):strToSybol rs (c+1))
                    | h == "(" = let    exp = getNext (h:rs) [] 0
                                in
                                    ((Symbol "" (createExpression (fst exp))):strToSybol (snd exp) (c+1))

defineCall :: [String] -> [String] -> Int -> Expression
defineCall [] _ _ = (Value (ValueError (Error 90)) "")
defineCall a b 0    | (head a) == "(" = let    name = getName (a)
                                        in
                                            (Define (fst name) (Lambda (strToSybol (snd name) 0) [(createExpression b)]))
                    | onlyNumbers (head a) == False && isBool (head a) == False = let name = getName a
                                                                    in
                                                                        (Define (head a) (createExpression b))

getName :: [String] -> (String, [String])
getName ("(":rs) = getName rs
getName (h:rs) = (h, ("(":rs))
getName [] = ("", [])