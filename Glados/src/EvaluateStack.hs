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
evaluateExpression exp' _ env = (head (snd result), fst result)
    where
        result =  eval (env, []) exp'

removeLastArray :: ([[Symbol]], [Value]) -> ([[Symbol]], [Value])
removeLastArray ((_:xs), stack) = (xs, stack)

eval :: ([[Symbol]], [Value]) -> Expression -> ([[Symbol]], [Value])
eval (env, stack) (Value (ValueError (Error 1)) name') = findSymbol env env stack name' []
eval (env, stack) (Value val _) = (env, val : stack)
eval (env, stack) (Lambda [] (last : [])) = eval (env, stack) last
eval ((x:xs), stack) (Lambda [] ((Define name' expr) : rest)) = eval ((((Symbol name' expr) : x) : xs), stack) (Lambda [] rest)
eval (env, stack) (Lambda [] (_ : rest)) = eval (env, stack) (Lambda [] rest)
eval (env, stack) (Lambda args functions) = removeLastArray (eval (args : env, stack) (Lambda [] functions))
eval (env, stack) (SymbolExpression name' args) = findSymbol env env stack name' args
eval (env, stack) (Plus expr1 expr2) = removeLastArray (applyop ([]:env) stack "Plus" expr1 expr2)
eval (env, stack) (Minus expr1 expr2) = removeLastArray (applyop ([]:env) stack "Minus" expr1 expr2)
eval (env, stack) (Times expr1 expr2) = removeLastArray (applyop ([]:env) stack "Times" expr1 expr2)
eval (env, stack) (Divided expr1 expr2) = removeLastArray (applyop ([]:env) stack "Divide" expr1 expr2)
eval (env, stack) (Modulo expr1 expr2) = removeLastArray (applyop ([]:env) stack "Modula" expr1 expr2)
eval ((x:xs), stack) (Define name' expr) = ((((Symbol name' expr):x) : xs), (ValueError (Error 0)) : stack)
eval (env, stack) (Equal expr1 expr2) = removeLastArray (applyop ([]:env) stack "Equal" expr1 expr2)
eval (env, stack) (Smaller expr1 expr2) = removeLastArray (applyop ([]:env) stack "Smaller" expr1 expr2)
eval (env, stack) (Condition ifExpr thenExpr elseExpr) = removeLastArray (applyCond ([]:env) (snd (eval (([]:env), stack) ifExpr)) thenExpr elseExpr)

findSymbol :: [[Symbol]] -> [[Symbol]] -> [Value] -> String -> [Symbol] -> ([[Symbol]], [Value])
findSymbol [] env' stack _ _ = (env', (ValueError (Error 80)) : stack)
findSymbol ([]:xs) env' stack name' args = findSymbol xs env' stack name' args
findSymbol (((Symbol name'' (Lambda args' exprs)) : _) : _) env' stack name' args
          | name' == name'' && length args == length args' = removeLastArray (eval (env', stack) (Lambda args' exprs))
          | name' == name'' && otherwise = (env', (ValueError (Error 85)) : stack)
findSymbol (((Symbol name'' expr) : _) : _) env' stack name' [] | name'' == name'  = eval (env', stack) expr
findSymbol (((Symbol name'' _) : _) : _) env' stack name' _ | name'' == name' = (env', (ValueError (Error 85)) : stack)
findSymbol ((_ : xs) : xss) env' stack name' args = findSymbol (xs : xss) env' stack name' args

applyCond :: [[Symbol]] -> [Value] -> Expression -> Expression -> ([[Symbol]], [Value])
applyCond env ((ValueBool True):xs) thenExpr _ = eval (env, xs) thenExpr
applyCond env ((ValueBool False):xs) _ elseExpr = eval (env, xs) elseExpr
applyCond env ((_):xs) _ _ = (env, ((ValueError (Error 82)) : xs))

applyop :: [[Symbol]] -> [Value] -> String -> Expression -> Expression -> ([[Symbol]], [Value])
applyop env stack op expr1 expr2 = (env''', (operate op v1 v2) : restStack'')
  where
    (env''', (v1:v2:restStack'')) = (env'', restStack')
        where
          (env'', restStack') = eval (env, restStack) expr1
            where
              (_, restStack) = eval (env, stack) expr2

operate :: String -> Value -> Value -> Value
operate _ (ValueError err) _ = (ValueError err)
operate _ _ (ValueError err) = (ValueError err)
operate "Equal" (ValueInt v1) (ValueInt v2)
      | v1 == v2 = (ValueBool True)
      | otherwise = (ValueBool False)
operate "Smaller" (ValueInt v1) (ValueInt v2)
      | v1 < v2 = (ValueBool True)
      | otherwise = (ValueBool False)
operate "Equal" (ValueBool v1) (ValueBool v2)
      | v1 == v2 = (ValueBool True)
      | otherwise = (ValueBool False)
operate "Plus" (ValueInt v1) (ValueInt v2) = (ValueInt (v1 + v2))
operate "Minus" (ValueInt v1) (ValueInt v2) = (ValueInt (v1 - v2))
operate "Times" (ValueInt v1) (ValueInt v2) = (ValueInt (v1 * v2))
operate "Divide" (ValueInt _) (ValueInt 0) = (ValueError (Error 83))
operate "Modula" (ValueInt _) (ValueInt 0) = (ValueError (Error 83))
operate "Divide" (ValueInt v1) (ValueInt v2) = (ValueInt (div v1 v2))
operate "Modula" (ValueInt v1) (ValueInt v2) = (ValueInt (mod v1 v2))
operate _ _ _ = (ValueError (Error 82))