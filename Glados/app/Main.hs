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

-- main :: IO ()
-- main =  do  someFunc
--             testFunc1
--             testFunc2

data Symbol = Symbol {name :: String, rep :: Expression}

data Error = Error Int deriving Show

data Value = ValueInt Int | ValueBool Bool| ValueError Error deriving Show

data Expression =   Value {value :: Value, valueName :: String} |
                    Lambda {args :: [String], function :: [Expression]} |
                    Plus {firstValuePlus :: Expression, secondValuePlus :: Expression} |
                    Minus {firstValueMinus :: Expression, secondValueMinus :: Expression} |
                    Divided {firstValueDivided :: Expression, secondValueDivided :: Expression} |
                    Times {firstValueTimes :: Expression, secondValueTimes :: Expression} |
                    Modulo {firstValueModulo :: Expression, secondValueModulo :: Expression} |
                    Define {defineName :: String, defined :: Expression} |
                    Equal {firstValueEqual :: Expression, secondValueEqual :: Expression} |
                    Smaller {firstValueSmaller :: Expression, secondValueSmaller :: Expression} |
                    Condition {ifValue :: Expression, thenExpression :: Expression, elseExpression :: Expression}

evaluatePlus :: (Value, [Symbol]) -> (Value, [Symbol]) -> (Value, [Symbol])
evaluatePlus ((ValueError err), symbols) (_, _) = ((ValueError err), symbols)
evaluatePlus (_, symbols) ((ValueError err), _) = ((ValueError err), symbols)
evaluatePlus ((ValueInt int1), symbols) ((ValueInt int2), _) = ((ValueInt (int1 + int2)), symbols)
evaluatePlus (_, symbols) (_, _) = ((ValueError (Error 84)), symbols)

evaluateMinus :: (Value, [Symbol]) -> (Value, [Symbol]) -> (Value, [Symbol])
evaluateMinus ((ValueError err), symbols) (_, _) = ((ValueError err), symbols)
evaluateMinus (_, symbols) ((ValueError err), _) = ((ValueError err), symbols)
evaluateMinus ((ValueInt int1), symbols) ((ValueInt int2), _) = ((ValueInt (int1 - int2)), symbols)
evaluateMinus (_, symbols) (_, _) = ((ValueError (Error 84)), symbols)

evaluateTimes :: (Value, [Symbol]) -> (Value, [Symbol]) -> (Value, [Symbol])
evaluateTimes ((ValueError err), symbols) (_, _) = ((ValueError err), symbols)
evaluateTimes (_, symbols) ((ValueError err), _) = ((ValueError err), symbols)
evaluateTimes ((ValueInt int1), symbols) ((ValueInt int2), _) = ((ValueInt (int1 * int2)), symbols)
evaluateTimes (_, symbols) (_, _) = ((ValueError (Error 84)), symbols)

evaluateDivided :: (Value, [Symbol]) -> (Value, [Symbol]) -> (Value, [Symbol])
evaluateDivided ((ValueError err), symbols) (_, _) = ((ValueError err), symbols)
evaluateDivided (_, symbols) ((ValueError err), _) = ((ValueError err), symbols)
evaluateDivided ((ValueInt int), symbols) ((ValueInt 0), _) = ((ValueError (Error 84)), symbols)
evaluateDivided ((ValueInt int1), symbols) ((ValueInt int2), _) = ((ValueInt (div int1 int2)), symbols)
evaluateDivided (_, symbols) (_, _) = ((ValueError (Error 84)), symbols)

evaluateModulo :: (Value, [Symbol]) -> (Value, [Symbol]) -> (Value, [Symbol])
evaluateModulo ((ValueError err), symbols) (_, _) = ((ValueError err), symbols)
evaluateModulo (_, symbols) ((ValueError err), _) = ((ValueError err), symbols)
evaluateModulo ((ValueInt int), symbols) ((ValueInt 0), _) = ((ValueError (Error 84)), symbols)
evaluateModulo ((ValueInt int1), symbols) ((ValueInt int2), _) = ((ValueInt (mod int1 int2)), symbols)
evaluateModulo (_, symbols) (_, _) = ((ValueError (Error 84)), symbols)

returnValue :: Expression -> Value
returnValue (Value val _) = val
returnValue _ = (ValueError (Error 84))

findValue :: String -> [Symbol] -> Value
findValue name1 [] = (ValueError (Error 84))
findValue name1 (x:xs) | name x == name1 = returnValue (rep x)
findValue name1 (x:xs) | otherwise = findValue name1 xs

evaluateValue :: Value -> String -> [Symbol] -> Value
evaluateValue (ValueInt int) _ symbols = (ValueInt int)
evaluateValue (ValueBool bool) _ symbols = (ValueBool bool)
evaluateValue (ValueError (Error 1)) name1 symbols = findValue name1 symbols
evaluateValue val _ _ = val

evaluateExpression :: Expression -> Int -> [Symbol] -> (Value, [Symbol])
evaluateExpression _ 100 symbols = ((ValueError (Error 84)), symbols)
evaluateExpression (Value val name1) recursion symbols = ((evaluateValue val name1 symbols) , symbols)
evaluateExpression (Plus exp1 exp2) recursion symbols = evaluatePlus (evaluateExpression exp1 (recursion + 1) symbols) (evaluateExpression exp2 (recursion + 1) symbols)
evaluateExpression (Minus exp1 exp2) recursion symbols = evaluateMinus (evaluateExpression exp1 (recursion + 1) symbols) (evaluateExpression exp2 (recursion + 1) symbols)
evaluateExpression (Times exp1 exp2) recursion symbols = evaluateTimes (evaluateExpression exp1 (recursion + 1) symbols) (evaluateExpression exp2 (recursion + 1) symbols)
evaluateExpression (Divided exp1 exp2) recursion symbols = evaluateDivided (evaluateExpression exp1 (recursion + 1) symbols) (evaluateExpression exp2 (recursion + 1) symbols)
evaluateExpression (Modulo exp1 exp2) recursion symbols = evaluateModulo (evaluateExpression exp1 (recursion + 1) symbols) (evaluateExpression exp2 (recursion + 1) symbols)
evaluateExpression (Define name1 exp1) recursion symbols = ((ValueError (Error 0)), symbols ++ [Symbol name1 exp1])

main :: IO ()
main = print (fst (evaluateExpression (Plus (Value (ValueError (Error 1)) "first") (Value (ValueInt 4) "second")) 0 [(Symbol "first" (Value (ValueInt 3) ""))]))
