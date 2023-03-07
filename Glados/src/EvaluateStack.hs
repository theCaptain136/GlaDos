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
eval (env, stack) (Value (ValueError (Error 1 empty)) name') = findSymbol env env stack name' [] (Value (ValueError (Error 1 empty)))
eval (env, stack) (Value val _) = (env, val : stack)
eval (env, stack) (Lambda [] (last : [])) = eval (env, stack) last
eval ((x:xs), stack) (Lambda [] ((Define name' expr) : rest)) = eval ((((Symbol name' expr) : x) : xs), stack) (Lambda [] rest)
eval (env, stack) (Lambda [] (_ : rest)) = eval (env, stack) (Lambda [] rest)
eval ((x:xs), stack) (Lambda args functions) = removeLastArray (eval (([]:(args ++ x):xs), stack) (Lambda [] functions))
eval (env, stack) (SymbolExpression name' args) = findSymbol env env stack name' args
eval (env, stack) (Plus expr1 expr2) = removeLastArray (applyop ([]:env) stack "Plus" expr1 expr2 (Plus expr1 expr2))
eval (env, stack) (Minus expr1 expr2) = removeLastArray (applyop ([]:env) stack "Minus" expr1 expr2 (Minus expr1 expr2))
eval (env, stack) (Times expr1 expr2) = removeLastArray (applyop ([]:env) stack "Times" expr1 expr2 (Times expr1 expr2))
eval (env, stack) (Divided expr1 expr2) = removeLastArray (applyop ([]:env) stack "Divide" expr1 expr2 (Divided expr1 expr2))
eval (env, stack) (Modulo expr1 expr2) = removeLastArray (applyop ([]:env) stack "Modula" expr1 expr2 (Modulo expr1 expr2))
eval ((x:xs), stack) (Define name' expr) = ((((Symbol name' expr):x) : xs), (ValueError (Error 0)) : stack)
eval (env, stack) (Equal expr1 expr2) = removeLastArray (applyop ([]:env) stack "Equal" expr1 expr2 (Equal expr1 expr2))
eval (env, stack) (Smaller expr1 expr2) = removeLastArray (applyop ([]:env) stack "Smaller" expr1 expr2 (Smaller expr1 expr2))
eval (env, stack) (Condition ifExpr thenExpr elseExpr) = removeLastArray (applyCond ([]:env) (snd (eval (([]:env), stack) ifExpr)) thenExpr elseExpr (Condition ifExpr thenExpr elseExpr))

fuseArgs :: [Symbol] -> [Symbol] -> [Symbol]
fuseArgs [] _ = []
fuseArgs ((Symbol name' (Value (ValueError (Error 1 _)) _)):xs) [] = []
fuseArgs ((Symbol name' (Value (ValueError (Error 1 _)) _)):xs) (y:ys) = (Symbol name' (rep y)) : (fuseArgs xs ys)
fuseArgs ((Symbol name' expr'):xs) args = (Symbol name' expr') : (fuseArgs xs args)

findSymbol :: [[Symbol]] -> [[Symbol]] -> [Value] -> String -> [Symbol] -> Expression -> ([[Symbol]], [Value])
findSymbol [] env' stack _ _ err = (env', (ValueError (Error 80 err)) : stack)
findSymbol ([]:xs) env' stack name' args err = findSymbol xs env' stack name' args err
findSymbol (((Symbol name'' (Lambda args' exprs)) : _) : _) env' stack name' args _
          | name' == name'' = removeLastArray (eval (env', stack) (Lambda (fuseArgs args' args) exprs))
findSymbol (((Symbol name'' expr) : _) : _) env' stack name' [] _ | name'' == name'  = eval (env', stack) expr
findSymbol (((Symbol name'' _) : _) : _) env' stack name' _ _ | name'' == name' = (env', (ValueError (Error 85 err)) : stack)
findSymbol ((_ : xs) : xss) env' stack name' args err = findSymbol (xs : xss) env' stack name' args err

applyCond :: [[Symbol]] -> [Value] -> Expression -> Expression -> Expression -> ([[Symbol]], [Value])
applyCond env ((ValueBool True):xs) thenExpr _ _ = eval (env, xs) thenExpr
applyCond env ((ValueBool False):xs) _ elseExpr _ = eval (env, xs) elseExpr
applyCond env ((_):xs) _ _ err = (env, ((ValueError (Error 82 err)) : xs))

applyop :: [[Symbol]] -> [Value] -> String -> Expression -> Expression -> Expression -> ([[Symbol]], [Value])
applyop env stack op expr1 expr2 err = (env''', (operate op v1 v2 err) : restStack'')
  where
    (env''', (v1:v2:restStack'')) = (env'', restStack')
      where
        (env'', restStack') = eval (env, restStack) expr1
          where
            (_, restStack) = eval (env, stack) expr2

operate :: String -> Value -> Value -> Expression -> Value
operate _ (ValueError err) _ _ = (ValueError err)
operate _ _ (ValueError err) _ = (ValueError err)
operate "Equal" (ValueInt v1) (ValueInt v2) _
      | v1 == v2 = (ValueBool True)
      | otherwise = (ValueBool False)
operate "Smaller" (ValueInt v1) (ValueInt v2) _
      | v1 < v2 = (ValueBool True)
      | otherwise = (ValueBool False)
operate "Plus" (ValueInt v1) (ValueInt v2) _ = (ValueInt (v1 + v2))
operate "Minus" (ValueInt v1) (ValueInt v2) _ = (ValueInt (v1 - v2))
operate "Times" (ValueInt v1) (ValueInt v2) _ = (ValueInt (v1 * v2))
operate "Divide" (ValueInt _) (ValueInt 0) err = (ValueError (Error 83 err))
operate "Modula" (ValueInt _) (ValueInt 0) err = (ValueError (Error 83 err))
operate "Divide" (ValueInt v1) (ValueInt v2) _ = (ValueInt (div v1 v2))
operate "Modula" (ValueInt v1) (ValueInt v2) _ = (ValueInt (mod v1 v2))
operate _ _ _ err = (ValueError (Error 82 err))