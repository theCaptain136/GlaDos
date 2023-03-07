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

import Data.List

-- The rep of a Symbol can be any Expression. In case of a value it is for example (Value (ValueInt 69)).

data Symbol = Symbol {name :: String, rep :: Expression}

instance Show Symbol where
    show (Symbol name rep) = "(" ++ show name ++ " " ++ show rep ++ ")"

-- Error is used as a data type to determine what error caused the programm to exit.
-- When an error occurs the (ValueError (Error x)) will be returned.
-- When an evaluate function has to evaluate a error it will instead return the error.
-- This means the Ast will still be recursivly evaluated after an error occured, just in the fasted exit way possible.

-- The error code 1 is used when a value doesn't have a actual value yet, but will be available as a symbol during execution.
-- In that case it will search the symbols for a symbol with the same name as the value. If a function is found first, it will result in an error.

data Error = Error {code :: Int, cause :: Expression}

instance Show Error where
    show (Error code cause) = "Error Code: " ++ show code ++ " occured in: " ++ show cause

-- Error list:
-- 0 = OK (used as a return value for define)
-- 1 = Value is stored as a symbol
-- 80 = Value not defined
-- 81 = recursion limit reached
-- 82 = invalid comparison
-- 83 = invalid mathematic action
-- 84 = invalid use data type
-- 85 = invalid number of args
-- 86 = invalid syntax
-- 87 = expected a value

-- Value will be every type of value possible and then can be used for patternmatching during evaluation.
-- e.g.: Plus expects two values as input, but with patternmatching inside the function it will be determined if you try add a Int to a Bool.

data Value = ValueInt Int | ValueBool Bool| ValueError Error deriving Show

-- Every Expression can be evaluated with the evaluateExpression function.
-- SymbolExpression is used when you want to evaluate a Symbol. The argsSymbol are used if you want to execute a lambda function which needs arguments. In that case they will be assigned in order.
-- The Lambda is used for some kind of lambda function execution. It will be used to store the information if you assign a lambda function to a symbol. 
--  The argsSymbol are the input of the lambda function and will be put into the main [[Symbol]] for the scope of this lambda. In function field a list of Expressions are stored, that will be executed in order. Only the return value of the last function will be returned by the lambda.
-- The Plus, Minus, Divided, Times and Mosulo types define what kind of mathematical operation will be executed with the firstValue and SecondValue.
-- The Define function creates a Symbol in the main [[Symbol]] and the. returns the (ErrorValue (Error 0)).
-- The Equal and Smaller types will try to compare the firstValue and secondValue and return the corresponding Bool.
-- The Condition type will evaluate the ifValue and depending of the returned Bool it will evaluate the thenExpression (True) or the elseExpression (False).

data Expression =   Value {value :: Value, valueName :: String} |
                    SymbolExpression {symbolName :: String, argsSymbol :: [Symbol]} |
                    Lambda {argsLambda :: [Symbol], function :: [Expression]} |
                    Plus {firstValuePlus :: Expression, secondValuePlus :: Expression} |
                    Minus {firstValueMinus :: Expression, secondValueMinus :: Expression} |
                    Divided {firstValueDivided :: Expression, secondValueDivided :: Expression} |
                    Times {firstValueTimes :: Expression, secondValueTimes :: Expression} |
                    Modulo {firstValueModulo :: Expression, secondValueModulo :: Expression} |
                    Define {defineName :: String, defined :: Expression} |
                    Equal {firstValueEqual :: Expression, secondValueEqual :: Expression} |
                    Smaller {firstValueSmaller :: Expression, secondValueSmaller :: Expression} |
                    Condition {ifValue :: Expression, thenExpression :: Expression, elseExpression :: Expression} |
                    Empty Int

instance Show Expression where
    show (Value value valueName) = "(" ++ valueName ++ ":" ++ show value ++ ")"
    show (SymbolExpression symbolName argsSymbol) = "(" ++ symbolName ++ "(" ++ intercalate "," (map show argsSymbol) ++ ")" ++ ")"
    show (Lambda args functions) = "(lambda" ++ "(" ++ intercalate "," (map show args) ++ ")" ++ "(" ++ intercalate "," (map show functions) ++ "))"
    show (Plus a b) = "(" ++ show a ++ " + " ++ show b ++ ")"
    show (Minus a b) = "(" ++ show a ++ " - " ++ show b ++ ")"
    show (Divided a b) = "(" ++ show a ++ " / " ++ show b ++ ")"
    show (Times a b) = "(" ++ show a ++ " * " ++ show b ++ ")"
    show (Modulo a b) = "(" ++ show a ++ " % " ++ show b ++ ")"
    show (Define name defined) = "(" ++ name ++ " = " ++ show defined ++ ")"
    show (Equal a b) = "(" ++ show a ++ " == " ++ show b ++ ")"
    show (Smaller a b) = "(" ++ show a ++ " < " ++ show b ++ ")"
    show (Condition i t e) = "(if " ++ show i ++ " then " ++ show t ++ " else " ++ show e ++ ")"
    show (Empty n) = "empty " ++ show n
