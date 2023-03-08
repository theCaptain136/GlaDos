--
-- EPITECH PROJECT, 2023
-- GlaDos
-- File description:
-- Evaluate
--

module Evaluate (
    evaluateExpression
    ) where

import AST

evaluatePlus :: (Value, [[Symbol]]) -> (Value, [[Symbol]]) -> Expression -> (Value, [[Symbol]])
evaluatePlus ((ValueError (Error 0 _)), symbols) (_, _) err = ((ValueError (Error 83 err)), symbols)
evaluatePlus (_, symbols) ((ValueError (Error 0 _)), _) err = ((ValueError (Error 83 err)), symbols)
evaluatePlus ((ValueError err), symbols) (_, _) _ = ((ValueError err), symbols)
evaluatePlus (_, symbols) ((ValueError err), _) _ = ((ValueError err), symbols)
evaluatePlus ((ValueInt int1), symbols) ((ValueInt int2), _) _ = ((ValueInt (int1 + int2)), symbols)
evaluatePlus (_, symbols) (_, _) err = ((ValueError (Error 83 err)), symbols)

evaluateMinus :: (Value, [[Symbol]]) -> (Value, [[Symbol]]) -> Expression -> (Value, [[Symbol]])
evaluateMinus ((ValueError (Error 0 _)), symbols) (_, _) err = ((ValueError (Error 83 err)), symbols)
evaluateMinus (_, symbols) ((ValueError (Error 0 _)), _) err = ((ValueError (Error 83 err)), symbols)
evaluateMinus ((ValueError err), symbols) (_, _) _ = ((ValueError err), symbols)
evaluateMinus (_, symbols) ((ValueError err), _) _ = ((ValueError err), symbols)
evaluateMinus ((ValueInt int1), symbols) ((ValueInt int2), _) _ = ((ValueInt (int1 - int2)), symbols)
evaluateMinus (_, symbols) (_, _) err = ((ValueError (Error 83 err)), symbols)

evaluateTimes :: (Value, [[Symbol]]) -> (Value, [[Symbol]]) -> Expression -> (Value, [[Symbol]])
evaluateTimes ((ValueError (Error 0 _)), symbols) (_, _) err = ((ValueError (Error 83 err)), symbols)
evaluateTimes (_, symbols) ((ValueError (Error 0 _)), _) err = ((ValueError (Error 83 err)), symbols)
evaluateTimes ((ValueError err), symbols) (_, _) _ = ((ValueError err), symbols)
evaluateTimes (_, symbols) ((ValueError err), _) _ = ((ValueError err), symbols)
evaluateTimes ((ValueInt int1), symbols) ((ValueInt int2), _) _ = ((ValueInt (int1 * int2)), symbols)
evaluateTimes (_, symbols) (_, _) err = ((ValueError (Error 83 err)), symbols)

evaluateDivided :: (Value, [[Symbol]]) -> (Value, [[Symbol]]) -> Expression -> (Value, [[Symbol]])
evaluateDivided ((ValueError (Error 0 _)), symbols) (_, _) err = ((ValueError (Error 83 err)), symbols)
evaluateDivided (_, symbols) ((ValueError (Error 0 _)), _) err = ((ValueError (Error 83 err)), symbols)
evaluateDivided ((ValueError err), symbols) (_, _) _ = ((ValueError err), symbols)
evaluateDivided (_, symbols) ((ValueError err), _) _ = ((ValueError err), symbols)
evaluateDivided ((ValueInt int), symbols) ((ValueInt 0), _) err = ((ValueError (Error 83 err)), symbols)
evaluateDivided ((ValueInt int1), symbols) ((ValueInt int2), _) _ = ((ValueInt (div int1 int2)), symbols)
evaluateDivided (_, symbols) (_, _) err = ((ValueError (Error 83 err)), symbols)

evaluateModulo :: (Value, [[Symbol]]) -> (Value, [[Symbol]]) -> Expression-> (Value, [[Symbol]])
evaluateModulo ((ValueError (Error 0 _)), symbols) (_, _) err = ((ValueError (Error 83 err)), symbols)
evaluateModulo (_, symbols) ((ValueError (Error 0 _)), _) err = ((ValueError (Error 83 err)), symbols)
evaluateModulo ((ValueError err), symbols) (_, _) _ = ((ValueError err), symbols)
evaluateModulo (_, symbols) ((ValueError err), _) _ = ((ValueError err), symbols)
evaluateModulo ((ValueInt int), symbols) ((ValueInt 0), _) err = ((ValueError (Error 83 err)), symbols)
evaluateModulo ((ValueInt int1), symbols) ((ValueInt int2), _) _ = ((ValueInt (mod int1 int2)), symbols)
evaluateModulo (_, symbols) (_, _) err = ((ValueError (Error 83 err)), symbols)

findSymbol :: String -> [[Symbol]] -> Expression -> Symbol
findSymbol name1 [] err = (Symbol "" (Value (ValueError (Error 80 err)) ""))
findSymbol name1 ([]:xs) err = findSymbol name1 xs err
findSymbol name1 (x:xs) err = findSymbol2 name1 x xs err

findSymbol2 :: String -> [Symbol] -> [[Symbol]] -> Expression -> Symbol
findSymbol2 name1 [] rest err = findSymbol name1 rest err
findSymbol2 name1 (x:xs) rest _ | name x == name1 = x
findSymbol2 name1 (x:xs) rest err | otherwise = findSymbol2 name1 xs rest err

findValue :: String -> [[Symbol]] -> [[Symbol]] -> Expression -> Value
findValue name1 [] symbols err = (ValueError (Error 80 err))
findValue name1 ([]:xs) symbols err = findValue name1 xs symbols err
findValue name1 (x:xs) symbols err = findValue2 name1 x xs symbols err

findValue2 :: String -> [Symbol] -> [[Symbol]] -> [[Symbol]] -> Expression -> Value
findValue2 name1 [] rest symbols err = findValue name1 rest symbols err
findValue2 name1 (x:xs) rest symbols _ | name x == name1 = (fst (evaluateExpression (rep x) 0 symbols))
findValue2 name1 (x:xs) rest symbols err | otherwise = findValue2 name1 xs rest symbols err

evaluateValue :: Value -> String -> [[Symbol]] -> Expression -> Value
evaluateValue (ValueInt int) _ symbols _ = (ValueInt int)
evaluateValue (ValueBool bool) _ symbols _ = (ValueBool bool)
evaluateValue (ValueError (Error 1 _)) name1 symbols err = findValue name1 symbols symbols err
evaluateValue val _ _ _ = val

evaluateCondition :: (Value, [[Symbol]]) -> Expression -> Expression -> Int -> Expression -> (Value, [[Symbol]])
evaluateCondition ((ValueBool True), symbols) exp2 exp3 recursion _ = (evaluateExpression exp2 (recursion + 1) symbols)
evaluateCondition ((ValueBool False), symbols) exp2 exp3 recursion _ = (evaluateExpression exp3 (recursion + 1) symbols)
evaluateCondition ((ValueError (Error 80 err)), symbols) _ _ _ _ = ((ValueError (Error 80 err)), symbols)
evaluateCondition ((ValueError (Error 81 err)), symbols) _ _ _ _ = ((ValueError (Error 81 err)), symbols)
evaluateCondition ((ValueError (Error 83 err)), symbols) _ _ _ _ = ((ValueError (Error 83 err)), symbols)
evaluateCondition (val, symbols) _ _ _ err = (ValueError (Error 82 err), symbols)

evaluateEqual :: (Value, [[Symbol]]) -> (Value, [[Symbol]]) -> Expression -> (Value, [[Symbol]])
evaluateEqual ((ValueError err), symbols) _ _ = ((ValueError err), symbols)
evaluateEqual _ ((ValueError err), symbols) _ = ((ValueError err), symbols)
evaluateEqual ((ValueInt val1), symbols) ((ValueInt val2), _) _
                | val1 == val2 = ((ValueBool True), symbols)
                | otherwise = ((ValueBool False), symbols)
evaluateEqual ((ValueBool val1), symbols) ((ValueBool val2), _) _
                | val1 == val2 = ((ValueBool True), symbols)
                | otherwise = ((ValueBool False), symbols)
evaluateEqual (_, symbols) _ err = ((ValueError (Error 82 err)), symbols)

evaluateSmaller :: (Value, [[Symbol]]) -> (Value, [[Symbol]]) -> Expression -> (Value, [[Symbol]])
evaluateSmaller ((ValueError err), symbols) _ _ = ((ValueError err), symbols)
evaluateSmaller _ ((ValueError err), symbols) _ = ((ValueError err), symbols)
evaluateSmaller ((ValueInt val1), symbols) ((ValueInt val2), _) _
                | val1 < val2 = ((ValueBool True), symbols)
                | otherwise = ((ValueBool False), symbols)
evaluateSmaller (_, symbols) _ err = ((ValueError (Error 82 err)), symbols)

evaluateLambda :: [[Symbol]] -> [Expression] -> Int -> (Value, [[Symbol]])
evaluateLambda symbols _ 100 = ((ValueError (Error 81 (Empty 0))), symbols)
evaluateLambda symbols (x:[]) recursion = evaluateExpression x (recursion + 1) symbols
evaluateLambda symbols (x:xs) recursion = evaluateLambda (snd (evaluateExpression x recursion symbols)) xs (recursion + 1)

addArgs :: [[Symbol]] -> [Symbol] -> [Symbol] -> [[Symbol]]
addArgs symbols [] [] = symbols
addArgs (x:xs) (y:ys) (z:zs) = addArgs (((Symbol (name y) (rep z) ) : x) : xs) ys zs

evaluateSymbol :: Symbol -> [Symbol] -> [[Symbol]] -> Int -> Expression -> (Value, [[Symbol]])
evaluateSymbol (Symbol name1 (Value (ValueError (Error err err')) _)) _ symbols _ _ = ((ValueError (Error err err')), symbols)
evaluateSymbol (Symbol name1 (Lambda args1 expressions)) args2 symbols recursion err | length args1 == length args2 = evaluateLambda (addArgs symbols args1 args2) expressions recursion
                                                                            | otherwise = ((ValueError (Error 85 err)), symbols)
evaluateSymbol (Symbol name1 exp1) [] symbols recursion _ = (evaluateExpression exp1 (recursion + 1) symbols)
evaluateSymbol (Symbol name1 exp1) args symbols recursion err = ((ValueError (Error 85 err)), symbols)

removeLastArray :: (Value, [[Symbol]]) -> (Value, [[Symbol]])
removeLastArray (val, (x:xs)) = (val, xs)

evaluateExpression :: Expression -> Int -> [[Symbol]] -> (Value, [[Symbol]])
evaluateExpression _ 100 symbols = ((ValueError (Error 81 (Empty 0))), symbols)
evaluateExpression (Value val name1) recursion symbols = ((evaluateValue val name1 symbols (Value val name1)) , symbols)
evaluateExpression (Plus exp1 exp2) recursion symbols = removeLastArray (evaluatePlus (evaluateExpression exp1 (recursion + 1) ([] : symbols)) (evaluateExpression exp2 (recursion + 1) ([] : symbols)) (Plus exp1 exp2))
evaluateExpression (Minus exp1 exp2) recursion symbols = removeLastArray (evaluateMinus (evaluateExpression exp1 (recursion + 1) ([] : symbols)) (evaluateExpression exp2 (recursion + 1) ([] : symbols)) (Minus exp1 exp2))
evaluateExpression (Times exp1 exp2) recursion symbols = removeLastArray (evaluateTimes (evaluateExpression exp1 (recursion + 1) ([] : symbols)) (evaluateExpression exp2 (recursion + 1) ([] : symbols)) (Times exp1 exp2))
evaluateExpression (Divided exp1 exp2) recursion symbols = removeLastArray (evaluateDivided (evaluateExpression exp1 (recursion + 1) ([] : symbols)) (evaluateExpression exp2 (recursion + 1) ([] : symbols)) (Divided exp1 exp2))
evaluateExpression (Modulo exp1 exp2) recursion symbols = removeLastArray (evaluateModulo (evaluateExpression exp1 (recursion + 1) ([] : symbols)) (evaluateExpression exp2 (recursion + 1) ([] : symbols)) (Modulo exp1 exp2))
evaluateExpression (Define name1 exp1) recursion (x:xs) = ((ValueError (Error 0 (Empty 0))), (((Symbol name1 exp1):x) : xs))
evaluateExpression (Condition exp1 exp2 exp3) recursion symbols = removeLastArray (evaluateCondition (evaluateExpression exp1 (recursion + 1) ([] : symbols)) exp2 exp3 recursion (Condition exp1 exp2 exp3))
evaluateExpression (Equal exp1 exp2) recursion symbols = removeLastArray (evaluateEqual (evaluateExpression exp1 (recursion + 1) ([] : symbols)) (evaluateExpression exp2 (recursion + 1) ([] : symbols)) (Equal exp1 exp2))
evaluateExpression (Smaller exp1 exp2) recursion symbols = removeLastArray (evaluateSmaller (evaluateExpression exp1 (recursion + 1) ([] : symbols)) (evaluateExpression exp2 (recursion + 1) ([] : symbols)) (Smaller exp1 exp2))
evaluateExpression (Lambda args expressions) recursion (x:xs) = removeLastArray (evaluateLambda ([]:(args ++ x):xs) expressions recursion)
evaluateExpression (SymbolExpression name1 args) recursion symbols = removeLastArray (evaluateSymbol (findSymbol name1 symbols (SymbolExpression name1 args)) args ([] : symbols) (recursion + 1) (SymbolExpression name1 args))
