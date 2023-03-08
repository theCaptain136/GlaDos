--
-- EPITECH PROJECT, 2023
-- B-FUN-500-BER-5-2-glados-pablo-elias.herrmann
-- File description:
-- Parser
--

module Parser
    ( testFunc1,
      parser,
      convert
    ) where

testFunc1 :: IO()
testFunc1 = putStrLn("hello from parser)")

convert :: [String] -> [String]
convert [] = []
convert (x:xs)  | x == "=" || x == "==" || x == "eq?" = ("eq?":convert xs)
convert (x:xs)  | x == "/" || x == "div" = ("div":convert xs)
convert (x:xs)  | x == "%" || x == "mod" = ("mod":convert xs)
convert (x:xs)  | x == "then" || x == "else" = (convert xs)
                | otherwise = (x:convert xs)

parser :: String -> String -> [String]
parser [] [] = []
parser [] save = save : parser [] []
parser (a:as) (save)    | a == ')' && save /= [] = (save : ")" : parser as [])
                        | a == '(' && save /= [] = (save : "(" : parser as [])
                        | a == '[' && save /= [] = (save : "[" : parser as [])
                        | a == ']' && save /= [] = (save : "]" : parser as [])
                        | a == ')' && save == [] = (")" : parser as [])
                        | a == '(' && save == [] = ("(" : parser as [])
                        | a == '[' && save == [] = ("[" : parser as [])
                        | a == ']' && save == [] = ("]" : parser as [])
                        | (a == ' ' || a == '\t' || a == '\n') && save /= [] = (save : parser as [])
                        | (a == ' ' || a == '\t' || a == '\n') && save == [] = parser as []
                        | otherwise = parser as (save ++ [a])

                    