--
-- EPITECH PROJECT, 2023
-- GlaDos
-- File description:
-- EvaluateStack
--

module Evaluate (
    evaluateExpression
    ) where

import AST

evaluateExpression :: Expression -> Int -> [[Symbol]] -> (Value, [[Symbol]])
evaluateExpression exp _ env = (fst (snd result), fst result)
    where
        result =  eval (env, []) exp

removeLastArray :: ([[Symbol]], [Value]) -> ([[Symbol]], [Value])
removeLastArray ((x:xs), stack) = (xs, stack)

-- Evaluate an expression in the given environment and stack
eval :: ([[Symbol]], [Value]) -> Expression -> ([[Symbol]], [Value])
eval (env, stack) (Value val name) = (env, Value val name : stack)
eval (env, stack) (Plus expr1 expr2) = removeLastArray (applyBinop ([]:env) stack (+) expr1 expr2)
eval (env, stack) (Minus expr1 expr2) = removeLastArray (applyBinop ([]:env) stack (-) expr1 expr2)
eval (env, stack) (Times expr1 expr2) = removeLastArray (applyBinop ([]:env) stack (*) expr1 expr2)
eval (env, stack) (Divided expr1 expr2) = removeLastArray (applyBinop ([]:env) stack div expr1 expr2)
eval (env, stack) (Modulo expr1 expr2) = removeLastArray (applyBinop ([]:env) stack mod expr1 expr2)
eval ((x:xs), stack) (Define name expr) = ((((Symbol name expr):x) : xs), (Value (ValueError (Error 0)) "") : stack)
eval (env, stack) (Equal expr1 expr2) = removeLastArray (applyBoolop ([]:env) stack "equal" expr1 expr2)
eval (env, stack) (Smaller expr1 expr2) = removeLastArray (applyBoolop ([]:env) stack "smaller" expr1 expr2)
eval (env, stack) (Condition ifExpr thenExpr elseExpr) = removeLastArray (applyCond ([]:env) stack (snd (eval (([]:env), stack) ifExpr)))

applyCond :: [[Symbol]] -> [Value] -> Expression -> Expression -> ([[Symbol]], [Value])
applyCond env ((ValueBool True):xs) thenExpr elseExpr = eval (env, xs) thenExpr
applyCond env ((ValueBool False):xs) thenExpr elseExpr = eval (env, xs) elseExpr
applyCond env ((_):xs) thenExpr elseExpr = (env, ((ValueError (Error 82)) : xs))

operate :: (Int -> Int -> Int) -> Value -> Value -> Value
operate _ (ValueError err) _ = (ValueError err)
operate _ _ (ValueError err) = (ValueError err)
operate (+) (ValueInt v1) (ValueInt v2) = (ValueInt (v1 + v2))
operate (-) (ValueInt v1) (ValueInt v2) = (ValueInt (v1 - v2))
operate (*) (ValueInt v1) (ValueInt v2) = (ValueInt (v1 * v2))
operate div (ValueInt v1) (ValueInt 0) = (ValueError (Error 83))
operate mod (ValueInt v1) (ValueInt 0) = (ValueError (Error 83))
operate div (ValueInt v1) (ValueInt v2) = (ValueInt (div v1 v2))
operate mod (ValueInt v1) (ValueInt v2) = (ValueInt (mod v1 v2))
operate _ _ _ = (ValueError (Error 83))

-- Apply a binary operator to the top two values on the stack
applyBinop :: [[Symbol]] -> [Value] -> (Int -> Int -> Int) -> Expression -> Expression -> ([[Symbol]], [Value])
applyBinop env stack op expr1 expr2 = (env'', (operate (op) v1 v2) : (tail (tail restStack)))
  where
    v1 = fst restStack
    v2 = snd restStack
    where
      (env'', restStack) = eval (env, restStack2) expr1
      where
        (env', restStack2) = eval (env, stack) expr2

operateBool :: String -> Value -> Value -> Value
operateBool _ (ValueError err) _ = (ValueError err)
operateBool _ _ (ValueError err) = (ValueError err)
operateBool "Equal" (ValueInt v1) (ValueInt v2)
      | v1 == v2 = (ValueBool True)
      | otherwise = (ValueBool False)
operateBool "Smaller" (ValueInt v1) (ValueInt v2)
      | v1 < v2 = (ValueBool True)
      | otherwise = (ValueBool False)
operateBool "Equal" (ValueBool v1) (ValueBool v2)
      | v1 == v2 = (ValueBool True)
      | otherwise = (ValueBool False)
operateBool _ _ _ = (ValueError (Error 82))

-- Apply a boolean operator to the top two values on the stack
applyBoolop :: [[Symbol]] -> [Value] -> String -> Expression -> Expression -> ([[Symbol]], [Value])
applyBinop env stack op expr1 expr2 = (env'', (operateBool (op) v1 v2) : (tail (tail restStack)))
  where
    v1 = fst restStack
    v2 = snd restStack
    where
      (env'', restStack) = eval (env, restStack2) expr1
      where
        (env', restStack2) = eval (env, stack) expr2