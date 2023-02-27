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
    strToSymbol,
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
                                | h == '(' = translator xs (depth+1) 0
                                | h == ')' = translator xs (depth-1) 1

translator ("+":a:b:xs) depth 0 = let   n = (getNext (a:b:xs) [] 0)
                                        n2 = ( getNext (snd n) [] 0)
                                    in
                                        (Plus (translator (fst n) depth 1) (translator (fst n2) depth 1)) -- Plus function

translator ("-":a:b:xs) depth 0 = let   n = (getNext (a:b:xs) [] 0)
                                        n2 = ( getNext (snd n) [] 0)
                                    in
                                        (Minus (translator (fst n) depth 1) (translator (fst n2) depth 1)) -- Minus function

translator ("*":a:b:xs) depth 0 = let   n = (getNext (a:b:xs) [] 0)
                                        n2 = ( getNext (snd n) [] 0)
                                    in
                                        (Times (translator (fst n) depth 1) (translator (fst n2) depth 1)) -- Times function

translator ("div":a:b:xs) depth 0 = let n = (getNext (a:b:xs) [] 0)
                                        n2 = ( getNext (snd n) [] 0)
                                    in
                                        (Divided (translator (fst n) depth 1) (translator (fst n2) depth 1)) -- Divided function

translator ("mod":a:b:xs) depth 0 = let n = (getNext (a:b:xs) [] 0)
                                        n2 = ( getNext (snd n) [] 0)
                                    in
                                        (Modulo (translator (fst n) depth 1) (translator (fst n2) depth 1)) -- Modulo function

translator ("eq?":a:b:xs) depth 0 = let n = (getNext (a:b:xs) [] 0)
                                        n2 = ( getNext (snd n) [] 0)
                                    in
                                        (Equal (translator (fst n) depth 1) (translator (fst n2) depth 1)) -- Equal function

translator ("<":a:b:xs) depth 0 = let   n = (getNext (a:b:xs) [] 0)
                                        n2 = ( getNext (snd n) [] 0)
                                    in
                                        (Smaller (translator (fst n) depth 1) (translator (fst n2) depth 1)) -- Smaller function


translator ("if":a:b:xs) depth 0 = let  n = (getNext (a:b:xs) [] 0)
                                        n2 = ( getNext (snd n) [] 0)
                                        n3 = ( getNext (snd n2) [] 0)
                                    in
                                        (Condition (translator (fst n) depth 1) (translator (fst n2) depth 1) (translator (fst n3) depth 1)) -- Condition function

translator ("define":a:b:xs) depth 0 = let  n = (getNext (a:b:xs) [] 0)
                                            n2 = ( getNext (snd n) [] 0)
                                    in
                                        (defineCall (fst n) (fst n2) 0)

translator ("lambda":a:b:xs) depth 0 = let  n = (getNext (a:b:xs) [] 0)
                                            n2 = (getNext (snd n) [] 0)
                                            arg = (strToSymbol (fst n) 0)
                                            sym =   if empty (snd n2)
                                                        then Nothing
                                                        else Just (strToSymbol (snd n2) 0)
                                            res = case sym of
                                                    Nothing -> arg
                                                    Just s -> giveNames arg s
                                    in
                                        (Lambda res [(createExpression (fst n2))])

translator (x:xs) depth 0 = SymbolExpression x (strToSymbol xs 0)    -- custom functions

translator (x:xs) depth 1   | (onlyNumbers x) == True = Value (ValueInt (stringToInt x)) "" -- only const values + variables
                            | (isBool x) == True = Value (ValueBool (toBool x)) ""
                            | otherwise = Value (ValueError (Error 1)) x

translator _ _ _ = (Value (ValueError (Error 900)) "error")

empty :: [String] -> Bool
empty (")":[]) = True
empty [] = True
empty _ = False

giveNames :: [Symbol] -> [Symbol] -> [Symbol] -- FIXME break if not enough args
giveNames [] _ = []
giveNames _ [] = []
giveNames ((Symbol aName aExp):as) ((Symbol bName bExp):bs) = ((Symbol aName bExp):(giveNames as bs))

maybeToList :: Maybe [Symbol] -> [Symbol]
maybeToList Nothing = []
maybeToList (Just symbols) = symbols

strToSymbol :: [String] -> Int -> [Symbol]
strToSymbol [] c = []
strToSymbol (")":[]) _ = []
strToSymbol (")":rs) 0 = strToSymbol rs 1
strToSymbol ("(":rs) 0 = strToSymbol rs 1
strToSymbol (h:rs) c | onlyNumbers h = ((Symbol "" (Value (ValueInt (stringToInt h)) "")):strToSymbol rs (c+1))
                    | isBool h = ((Symbol "" (Value (ValueBool (toBool h)) "")):strToSymbol rs (c+1))
                    | h /= "(" && h /= ")" = (Symbol h (Value (ValueError (Error 1)) h):strToSymbol rs (c+1))
                    | h == "(" = let    exp = getNext (h:rs) [] 0
                                in
                                    ((Symbol "" (createExpression (fst exp))):strToSymbol (snd exp) (c+1))

defineCall :: [String] -> [String] -> Int -> Expression
defineCall [] _ _ = (Value (ValueError (Error 90)) "")
defineCall a b 0    | (head a) == "(" = let    name = getName (a)
                                        in
                                            (Define (fst name) (Lambda (strToSymbol (snd name) 0) [(createExpression b)]))
                    | onlyNumbers (head a) == False && isBool (head a) == False = let name = getName a
                                                                    in
                                                                        (Define (head a) (createExpression b))

getName :: [String] -> (String, [String])
getName ("(":rs) = getName rs
getName (h:rs) = (h, ("(":rs))
getName [] = ("", [])