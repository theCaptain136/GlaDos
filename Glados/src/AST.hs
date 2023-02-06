--
-- EPITECH PROJECT, 2023
-- GlaDos
-- File description:
-- AST
--

module AST (
    Symbol (..),
    Error (..),
    Expression (..),
    Value (..)
    ) where

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
-- 86 = invalid syntax

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