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

-- Error list:
-- 0 = OK (used as a return value for define)
-- 1 = Value is stored as a symbol
-- 2 = Value is stored as symbol in this scope
-- 80 = Value not defined
-- 81 = recursion limit reached
-- 82 = invalid comparison
-- 83 = invalid mathematic action

data Value = ValueInt Int | ValueBool Bool| ValueError Error deriving Show

data Expression =   Value {value :: Value, valueName :: String} |
                    SymbolExpression {symbolName :: String, args :: [Expression]} |
                    Lambda {args :: [Expression], function :: [Expression]} |
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
evaluatePlus ((ValueError (Error 0)), symbols) (_, _) = ((ValueError (Error 83)), symbols)
evaluatePlus (_, symbols) ((ValueError (Error 0)), _) = ((ValueError (Error 83)), symbols)
evaluatePlus ((ValueError err), symbols) (_, _) = ((ValueError err), symbols)
evaluatePlus (_, symbols) ((ValueError err), _) = ((ValueError err), symbols)
evaluatePlus ((ValueInt int1), symbols) ((ValueInt int2), _) = ((ValueInt (int1 + int2)), symbols)
evaluatePlus (_, symbols) (_, _) = ((ValueError (Error 83)), symbols)

evaluateMinus :: (Value, [Symbol]) -> (Value, [Symbol]) -> (Value, [Symbol])
evaluateMinus ((ValueError (Error 0)), symbols) (_, _) = ((ValueError (Error 83)), symbols)
evaluateMinus (_, symbols) ((ValueError (Error 0)), _) = ((ValueError (Error 83)), symbols)
evaluateMinus ((ValueError err), symbols) (_, _) = ((ValueError err), symbols)
evaluateMinus (_, symbols) ((ValueError err), _) = ((ValueError err), symbols)
evaluateMinus ((ValueInt int1), symbols) ((ValueInt int2), _) = ((ValueInt (int1 - int2)), symbols)
evaluateMinus (_, symbols) (_, _) = ((ValueError (Error 83)), symbols)

evaluateTimes :: (Value, [Symbol]) -> (Value, [Symbol]) -> (Value, [Symbol])
evaluateTimes ((ValueError (Error 0)), symbols) (_, _) = ((ValueError (Error 83)), symbols)
evaluateTimes (_, symbols) ((ValueError (Error 0)), _) = ((ValueError (Error 83)), symbols)
evaluateTimes ((ValueError err), symbols) (_, _) = ((ValueError err), symbols)
evaluateTimes (_, symbols) ((ValueError err), _) = ((ValueError err), symbols)
evaluateTimes ((ValueInt int1), symbols) ((ValueInt int2), _) = ((ValueInt (int1 * int2)), symbols)
evaluateTimes (_, symbols) (_, _) = ((ValueError (Error 83)), symbols)

evaluateDivided :: (Value, [Symbol]) -> (Value, [Symbol]) -> (Value, [Symbol])
evaluateDivided ((ValueError (Error 0)), symbols) (_, _) = ((ValueError (Error 83)), symbols)
evaluateDivided (_, symbols) ((ValueError (Error 0)), _) = ((ValueError (Error 83)), symbols)
evaluateDivided ((ValueError err), symbols) (_, _) = ((ValueError err), symbols)
evaluateDivided (_, symbols) ((ValueError err), _) = ((ValueError err), symbols)
evaluateDivided ((ValueInt int), symbols) ((ValueInt 0), _) = ((ValueError (Error 83)), symbols)
evaluateDivided ((ValueInt int1), symbols) ((ValueInt int2), _) = ((ValueInt (div int1 int2)), symbols)
evaluateDivided (_, symbols) (_, _) = ((ValueError (Error 83)), symbols)

evaluateModulo :: (Value, [Symbol]) -> (Value, [Symbol]) -> (Value, [Symbol])
evaluateModulo ((ValueError (Error 0)), symbols) (_, _) = ((ValueError (Error 83)), symbols)
evaluateModulo (_, symbols) ((ValueError (Error 0)), _) = ((ValueError (Error 83)), symbols)
evaluateModulo ((ValueError err), symbols) (_, _) = ((ValueError err), symbols)
evaluateModulo (_, symbols) ((ValueError err), _) = ((ValueError err), symbols)
evaluateModulo ((ValueInt int), symbols) ((ValueInt 0), _) = ((ValueError (Error 83)), symbols)
evaluateModulo ((ValueInt int1), symbols) ((ValueInt int2), _) = ((ValueInt (mod int1 int2)), symbols)
evaluateModulo (_, symbols) (_, _) = ((ValueError (Error 83)), symbols)

returnValue :: Expression -> Value
returnValue (Value val _) = val
returnValue _ = (ValueError (Error 80))

findValue :: String -> [Symbol] -> Value
findValue name1 [] = (ValueError (Error 80))
findValue name1 (x:xs) | name x == name1 = returnValue (rep x)
findValue name1 (x:xs) | otherwise = findValue name1 xs

evaluateValue :: Value -> String -> [Symbol] -> Value
evaluateValue (ValueInt int) _ symbols = (ValueInt int)
evaluateValue (ValueBool bool) _ symbols = (ValueBool bool)
evaluateValue (ValueError (Error 1)) name1 symbols = findValue name1 symbols
evaluateValue val _ _ = val

evaluateCondition :: (Value, [Symbol]) -> Expression -> Expression -> Int -> (Value, [Symbol])
evaluateCondition ((ValueBool True), symbols) exp2 exp3 recursion = (evaluateExpression exp2 (recursion + 1) symbols)
evaluateCondition ((ValueBool False), symbols) exp2 exp3 recursion = (evaluateExpression exp3 (recursion + 1) symbols)
evaluateCondition ((ValueError (Error 80)), symbols) _ _ _ = ((ValueError (Error 80)), symbols)
evaluateCondition ((ValueError (Error 81)), symbols) _ _ _ = ((ValueError (Error 81)), symbols)
evaluateCondition ((ValueError (Error 83)), symbols) _ _ _ = ((ValueError (Error 83)), symbols)
evaluateCondition (val, symbols) _ _ _ = (ValueError (Error 82), symbols)

evaluateEqual :: (Value, [Symbol]) -> (Value, [Symbol]) -> (Value, [Symbol])
evaluateEqual ((ValueError err), symbols) _ = ((ValueError err), symbols)
evaluateEqual _ ((ValueError err), symbols) = ((ValueError err), symbols)
evaluateEqual ((ValueInt val1), symbols) ((ValueInt val2), _)
                | val1 == val2 = ((ValueBool True), symbols)
                | otherwise = ((ValueBool False), symbols)
evaluateEqual ((ValueBool val1), symbols) ((ValueBool val2), _)
                | val1 == val2 = ((ValueBool True), symbols)
                | otherwise = ((ValueBool False), symbols)
evaluateEqual (_, symbols) _ = ((ValueError (Error 82)), symbols)

evaluateSmaller :: (Value, [Symbol]) -> (Value, [Symbol]) -> (Value, [Symbol])
evaluateSmaller ((ValueError err), symbols) _ = ((ValueError err), symbols)
evaluateSmaller _ ((ValueError err), symbols) = ((ValueError err), symbols)
evaluateSmaller ((ValueInt val1), symbols) ((ValueInt val2), _)
                | val1 < val2 = ((ValueBool True), symbols)
                | otherwise = ((ValueBool False), symbols)
evaluateSmaller (_, symbols) _ = ((ValueError (Error 82)), symbols)

evaluateExpression :: Expression -> Int -> [Symbol] -> (Value, [Symbol])
evaluateExpression _ 100 symbols = ((ValueError (Error 81)), symbols)
evaluateExpression (Value val name1) recursion symbols = ((evaluateValue val name1 symbols) , symbols)
evaluateExpression (Plus exp1 exp2) recursion symbols = evaluatePlus (evaluateExpression exp1 (recursion + 1) symbols) (evaluateExpression exp2 (recursion + 1) symbols)
evaluateExpression (Minus exp1 exp2) recursion symbols = evaluateMinus (evaluateExpression exp1 (recursion + 1) symbols) (evaluateExpression exp2 (recursion + 1) symbols)
evaluateExpression (Times exp1 exp2) recursion symbols = evaluateTimes (evaluateExpression exp1 (recursion + 1) symbols) (evaluateExpression exp2 (recursion + 1) symbols)
evaluateExpression (Divided exp1 exp2) recursion symbols = evaluateDivided (evaluateExpression exp1 (recursion + 1) symbols) (evaluateExpression exp2 (recursion + 1) symbols)
evaluateExpression (Modulo exp1 exp2) recursion symbols = evaluateModulo (evaluateExpression exp1 (recursion + 1) symbols) (evaluateExpression exp2 (recursion + 1) symbols)
evaluateExpression (Define name1 exp1) recursion symbols = ((ValueError (Error 0)), symbols ++ [Symbol name1 exp1])
evaluateExpression (Condition exp1 exp2 exp3) recursion symbols = evaluateCondition (evaluateExpression exp1 (recursion + 1) symbols) exp2 exp3 recursion
evaluateExpression (Equal exp1 exp2) recursion symbols = evaluateEqual (evaluateExpression exp1 (recursion + 1) symbols) (evaluateExpression exp2 (recursion + 1) symbols)
evaluateExpression (Smaller exp1 exp2) recursion symbols = evaluateSmaller (evaluateExpression exp1 (recursion + 1) symbols) (evaluateExpression exp2 (recursion + 1) symbols)

main :: IO ()
main = print (fst (evaluateExpression (Plus (Value (ValueError (Error 1)) "first") (Value (ValueInt 4) "second")) 0 [(Symbol "first" (Value (ValueInt 3) ""))]))
