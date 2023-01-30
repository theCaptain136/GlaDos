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

evaluatePlus :: Value -> Value -> Value
evaluatePlus (ValueError err) _ = (ValueError err)
evaluatePlus _ (ValueError err) = (ValueError err)
evaluatePlus (ValueInt int1) (ValueInt int2) = (ValueInt (int1 + int2))
evaluatePlus _ _ = (ValueError (Error 84))

evaluateMinus :: Value -> Value -> Value
evaluateMinus (ValueError err) _ = (ValueError err)
evaluateMinus _ (ValueError err) = (ValueError err)
evaluateMinus (ValueInt int1) (ValueInt int2) = (ValueInt (int1 - int2))
evaluateMinus _ _ = (ValueError (Error 84))

evaluateTimes :: Value -> Value -> Value
evaluateTimes (ValueError err) _ = (ValueError err)
evaluateTimes _ (ValueError err) = (ValueError err)
evaluateTimes (ValueInt int1) (ValueInt int2) = (ValueInt (int1 * int2))
evaluateTimes _ _ = (ValueError (Error 84))

evaluateDivided :: Value -> Value -> Value
evaluateDivided (ValueError err) _ = (ValueError err)
evaluateDivided _ (ValueError err) = (ValueError err)
evaluateDivided (ValueInt int) (ValueInt 0) = (ValueError (Error 84))
evaluateDivided (ValueInt int1) (ValueInt int2) = (ValueInt (div int1 int2))
evaluateDivided _ _ = (ValueError (Error 84))

evaluateModulo :: Value -> Value -> Value
evaluateModulo (ValueError err) _ = (ValueError err)
evaluateModulo _ (ValueError err) = (ValueError err)
evaluateModulo (ValueInt int) (ValueInt 0) = (ValueError (Error 84))
evaluateModulo (ValueInt int1) (ValueInt int2) = (ValueInt (mod int1 int2))
evaluateModulo _ _ = (ValueError (Error 84))

evaluateExpression :: Expression -> Int -> Value
evaluateExpression _ 100 = (ValueError (Error 84))
evaluateExpression (Value val name) recursion = val
evaluateExpression (Plus exp1 exp2) recursion = evaluatePlus (evaluateExpression exp1 (recursion + 1)) (evaluateExpression exp2 (recursion + 1))
evaluateExpression (Minus exp1 exp2) recursion = evaluateMinus (evaluateExpression exp1 (recursion + 1)) (evaluateExpression exp2 (recursion + 1))
evaluateExpression (Times exp1 exp2) recursion = evaluateTimes (evaluateExpression exp1 (recursion + 1)) (evaluateExpression exp2 (recursion + 1))
evaluateExpression (Divided exp1 exp2) recursion = evaluateDivided (evaluateExpression exp1 (recursion + 1)) (evaluateExpression exp2 (recursion + 1))
evaluateExpression (Modulo exp1 exp2) recursion = evaluateModulo (evaluateExpression exp1 (recursion + 1)) (evaluateExpression exp2 (recursion + 1))

main :: IO ()
main = print (evaluateExpression (Plus (Value (ValueInt 5) "first") (Value (ValueInt 4) "second")) 0)
