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
createExpression scm = translator scm 0 1 Nothing

getNext :: [String] -> [String] -> Int -> ([String], [String])
getNext []  save c = (save, [])
getNext a b c | b /= [] && c == 0 = (b, a)
getNext ("(":xs) save c = getNext xs (save ++ ["("]) (c+1)
getNext (")":xs) save c = getNext xs (save ++ [")"]) (c-1)
getNext (x:xs) save c = getNext xs (save ++ [x]) c

getAtIndex :: [a] -> Int -> [a]
getAtIndex xs idx
    | idx >= 0 && idx < length xs = [(xs !! idx)]
    | otherwise = []

subStrEnd :: [a] -> Int -> [a]
subStrEnd [] _ = []
subStrEnd (x:xs) i  | i > 0 = subStrEnd xs (i-1)
                    | otherwise = (x:xs)

mhead :: [String] -> String
mhead (x:xs) = x
mhead [] = []

noMaybe :: Maybe Expression -> Expression
noMaybe x = let res =   case x of
                            Just a -> a
            in res

getValueString :: Expression -> String
getValueString (Value v str) = str

isError1 :: Expression -> Bool
isError1 (Value (ValueError (Error 1 _)) _) = True
isError1 _ = False

translator :: [String] -> Int -> Int -> Maybe Expression -> Expression
translator [] depth x p | depth /= 0 = (Value (ValueError (Error 86 (Empty 0))) "error")
translator (x:xs) depth c p | depth <= 0 && x == ")" = (Value (ValueError (Error 86 (Empty 0))) "error")
                            | x == "(" = let    n = getNext (x:xs) [] 0
                                                inf = (isInfix (mhead (snd n)))
                                                call =  if inf
                                                            then translator (snd n) (depth+1) 0 (Just (createExpression (fst n)))
                                                            else translator xs (depth+1) 0 Nothing
                                            in call
                            | x == ")" = translator xs (depth-1) 1 Nothing

translator ("=":a:b:xs) depth 0 p   | nothing p == False && isError1 (noMaybe p) = let  n = (getNext (a:b:xs) [] 0)
                                                                                        p2 = noMaybe p
                                    in (Define (getValueString (p2)) (createExpression (fst n))) -- Plus function

translator ("+":a:b:xs) depth 0 p = let n = (getNext (a:b:xs) [] 0)
                                        n2 = ( getNext (snd n) [] 0)
                                        exp1 =  case p of
                                                    Nothing  -> (translator (fst n) depth 1 p)
                                                    Just exp -> exp
                                        exp2 =  case p of
                                                    Nothing  -> (translator (fst n2) depth 1 p)
                                                    Just exp -> (translator (fst n) depth 1 p)
                                    in (Plus exp1 exp2) -- Plus function

translator ("-":a:b:xs) depth 0 p = let n = (getNext (a:b:xs) [] 0)
                                        n2 = ( getNext (snd n) [] 0)
                                        exp1 =  case p of
                                                    Nothing  -> (translator (fst n) depth 1 p)
                                                    Just exp -> exp
                                        exp2 =  case p of
                                                    Nothing  -> (translator (fst n2) depth 1 p)
                                                    Just exp -> (translator (fst n) depth 1 p)
                                    in (Minus exp1 exp2) -- Plus function

translator ("*":a:b:xs) depth 0 p = let n = (getNext (a:b:xs) [] 0)
                                        n2 = ( getNext (snd n) [] 0)
                                        exp1 =  case p of
                                                    Nothing  -> (translator (fst n) depth 1 p)
                                                    Just exp -> exp
                                        exp2 =  case p of
                                                    Nothing  -> (translator (fst n2) depth 1 p)
                                                    Just exp -> (translator (fst n) depth 1 p)
                                    in (Times exp1 exp2) -- Plus function

translator ("div":a:b:xs) depth 0 p = let   n = (getNext (a:b:xs) [] 0)
                                            n2 = ( getNext (snd n) [] 0)
                                            exp1 =  case p of
                                                        Nothing  -> (translator (fst n) depth 1 p)
                                                        Just exp -> exp
                                            exp2 =  case p of
                                                        Nothing  -> (translator (fst n2) depth 1 p)
                                                        Just exp -> (translator (fst n) depth 1 p)
                                    in (Divided exp1 exp2) -- Plus function

translator ("mod":a:b:xs) depth 0 p = let   n = (getNext (a:b:xs) [] 0)
                                            n2 = ( getNext (snd n) [] 0)
                                            exp1 =  case p of
                                                        Nothing  -> (translator (fst n) depth 1 p)
                                                        Just exp -> exp
                                            exp2 =  case p of
                                                        Nothing  -> (translator (fst n2) depth 1 p)
                                                        Just exp -> (translator (fst n) depth 1 p)
                                    in (Modulo exp1 exp2) -- Plus function

translator ("eq?":a:b:xs) depth 0 p = let   n = (getNext (a:b:xs) [] 0)
                                            n2 = ( getNext (snd n) [] 0)
                                            exp1 =  case p of
                                                        Nothing  -> (translator (fst n) depth 1 p)
                                                        Just exp -> exp
                                            exp2 =  case p of
                                                        Nothing  -> (translator (fst n2) depth 1 p)
                                                        Just exp -> (translator (fst n) depth 1 p)
                                    in (Equal exp1 exp2) -- Plus function

translator ("<":a:b:xs) depth 0 p = let n = (getNext (a:b:xs) [] 0)
                                        n2 = ( getNext (snd n) [] 0)
                                        exp1 =  case p of
                                                    Nothing  -> (translator (fst n) depth 1 p)
                                                    Just exp -> exp
                                        exp2 =  case p of
                                                    Nothing  -> (translator (fst n2) depth 1 p)
                                                    Just exp -> (translator (fst n) depth 1 p)
                                    in (Smaller exp1 exp2) -- Plus function

translator ("if":a:b:xs) depth 0 p = let    n = (getNext (a:b:xs) [] 0)
                                            n2 = ( getNext (snd n) [] 0)
                                            n3 = ( getNext (snd n2) [] 0)
                                    in
                                        (Condition (translator (fst n) depth 1 p) (translator (fst n2) depth 1 p) (translator (fst n3) depth 1 p)) -- Condition function

translator ("define":a:b:xs) depth 0 p = let    n = (getNext (a:b:xs) [] 0)
                                                n2 = ( getNext (snd n) [] 0)
                                    in
                                        (defineCall (fst n) (fst n2) 0)

translator ("lambda":a:b:xs) depth 0 p = let    n = (getNext (a:b:xs) [] 0)

                                                n2 = expArray (snd n) []
                                                arg = (strToSymbol (fst n) 0)
                                                sym =   if null (snd n2)
                                                            then Nothing
                                                            else Just (strToSymbol (snd n2) 0)
                                                res = case sym of
                                                        Nothing -> arg
                                                        Just s -> giveNames arg s
                                            in
                                                (Lambda res (fst n2) )

translator (x:xs) depth c p | onlyNumbers x && c == 0 && nothing p = (translator xs depth 0 (Just (Value (ValueInt (stringToInt x)) ""))) -- only const values + variables
                            | onlyNumbers x && c == 1 = (Value (ValueInt (stringToInt x)) "")
                            | (isBool x) && c == 0 && nothing p = (translator xs depth 0 (Just (Value (ValueBool (toBool x)) "")))
                            | (isBool x) && c == 1 = (Value (ValueBool (toBool x)) "")
                            | c == 1 = Value (ValueError (Error 1 (Empty 0))) x
                            | c == 0 && (isInfix (mhead xs)) = (translator xs depth 0 (Just (Value (ValueError (Error 1 (Empty 0))) x)))
                            | c == 0 = (SymbolExpression x (strToSymbol xs 0))

translator _ _ _ _ = (Value (ValueError (Error 88 (Empty 0))) "error")

expArray :: [String] -> [Expression] -> ([Expression], [String])
expArray [] e = (e, [])
expArray (")":xs) e = (e, xs)
expArray ("[":xs) e = (e, xs)
expArray (x:xs) e = let    (exp, rest) = getNext (x:xs) [] 0
                        in
                            expArray rest (e ++ [createExpression exp])

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
strToSymbol ("]":")":[]) _ = []
strToSymbol (")":rs) 0 = strToSymbol rs 1
strToSymbol ("(":rs) 0 = strToSymbol rs 1
strToSymbol (h:rs) c | onlyNumbers h = ((Symbol "" (Value (ValueInt (stringToInt h)) "")):strToSymbol rs (c+1))
                    | isBool h = ((Symbol "" (Value (ValueBool (toBool h)) "")):strToSymbol rs (c+1))
                    | h /= "(" && h /= ")" = (Symbol h (Value (ValueError (Error 1 (Empty 0))) h):strToSymbol rs (c+1))
                    | h == "(" = let    exp = getNext (h:rs) [] 0
                                in
                                    ((Symbol "" (createExpression (fst exp))):strToSymbol (snd exp) (c+1))

defineCall :: [String] -> [String] -> Int -> Expression
defineCall [] _ _ = (Value (ValueError (Error 90 (Empty 0))) "")
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

isInfix :: String -> Bool
isInfix expr =
  case expr of
    "=" -> True
    "+" -> True
    "-" -> True
    "+" -> True
    "div" -> True
    "mod" -> True
    _ -> False

nothing :: Maybe Expression -> Bool
nothing a =    let res =   case a of
                            Nothing -> True
                            Just a  -> False
        in
            res