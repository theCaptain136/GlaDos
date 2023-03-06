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

evaluatePlus :: (Value, [[Symbol]]) -> (Value, [[Symbol]]) -> (Value, [[Symbol]])
evaluatePlus ((ValueError (Error 0)), symbols) (_, _) = ((ValueError (Error 83)), symbols)
evaluatePlus (_, symbols) ((ValueError (Error 0)), _) = ((ValueError (Error 83)), symbols)
evaluatePlus ((ValueError err), symbols) (_, _) = ((ValueError err), symbols)
evaluatePlus (_, symbols) ((ValueError err), _) = ((ValueError err), symbols)
evaluatePlus ((ValueInt int1), symbols) ((ValueInt int2), _) = ((ValueInt (int1 + int2)), symbols)
evaluatePlus (_, symbols) (_, _) = ((ValueError (Error 83)), symbols)

evaluateMinus :: (Value, [[Symbol]]) -> (Value, [[Symbol]]) -> (Value, [[Symbol]])
evaluateMinus ((ValueError (Error 0)), symbols) (_, _) = ((ValueError (Error 83)), symbols)
evaluateMinus (_, symbols) ((ValueError (Error 0)), _) = ((ValueError (Error 83)), symbols)
evaluateMinus ((ValueError err), symbols) (_, _) = ((ValueError err), symbols)
evaluateMinus (_, symbols) ((ValueError err), _) = ((ValueError err), symbols)
evaluateMinus ((ValueInt int1), symbols) ((ValueInt int2), _) = ((ValueInt (int1 - int2)), symbols)
evaluateMinus (_, symbols) (_, _) = ((ValueError (Error 83)), symbols)

evaluateTimes :: (Value, [[Symbol]]) -> (Value, [[Symbol]]) -> (Value, [[Symbol]])
evaluateTimes ((ValueError (Error 0)), symbols) (_, _) = ((ValueError (Error 83)), symbols)
evaluateTimes (_, symbols) ((ValueError (Error 0)), _) = ((ValueError (Error 83)), symbols)
evaluateTimes ((ValueError err), symbols) (_, _) = ((ValueError err), symbols)
evaluateTimes (_, symbols) ((ValueError err), _) = ((ValueError err), symbols)
evaluateTimes ((ValueInt int1), symbols) ((ValueInt int2), _) = ((ValueInt (int1 * int2)), symbols)
evaluateTimes (_, symbols) (_, _) = ((ValueError (Error 83)), symbols)

evaluateDivided :: (Value, [[Symbol]]) -> (Value, [[Symbol]]) -> (Value, [[Symbol]])
evaluateDivided ((ValueError (Error 0)), symbols) (_, _) = ((ValueError (Error 83)), symbols)
evaluateDivided (_, symbols) ((ValueError (Error 0)), _) = ((ValueError (Error 83)), symbols)
evaluateDivided ((ValueError err), symbols) (_, _) = ((ValueError err), symbols)
evaluateDivided (_, symbols) ((ValueError err), _) = ((ValueError err), symbols)
evaluateDivided ((ValueInt int), symbols) ((ValueInt 0), _) = ((ValueError (Error 83)), symbols)
evaluateDivided ((ValueInt int1), symbols) ((ValueInt int2), _) = ((ValueInt (div int1 int2)), symbols)
evaluateDivided (_, symbols) (_, _) = ((ValueError (Error 83)), symbols)

evaluateModulo :: (Value, [[Symbol]]) -> (Value, [[Symbol]]) -> (Value, [[Symbol]])
evaluateModulo ((ValueError (Error 0)), symbols) (_, _) = ((ValueError (Error 83)), symbols)
evaluateModulo (_, symbols) ((ValueError (Error 0)), _) = ((ValueError (Error 83)), symbols)
evaluateModulo ((ValueError err), symbols) (_, _) = ((ValueError err), symbols)
evaluateModulo (_, symbols) ((ValueError err), _) = ((ValueError err), symbols)
evaluateModulo ((ValueInt int), symbols) ((ValueInt 0), _) = ((ValueError (Error 83)), symbols)
evaluateModulo ((ValueInt int1), symbols) ((ValueInt int2), _) = ((ValueInt (mod int1 int2)), symbols)
evaluateModulo (_, symbols) (_, _) = ((ValueError (Error 83)), symbols)

-- returnValue :: Expression -> Value
-- returnValue (Value val _) = val
-- returnValue _ = (ValueError (Error 84))

findSymbol :: String -> [[Symbol]] -> Symbol
findSymbol name1 [] = (Symbol "" (Value (ValueError (Error 80)) ""))
findSymbol name1 ([]:xs) = findSymbol name1 xs
findSymbol name1 (x:xs) = findSymbol2 name1 x xs

findSymbol2 :: String -> [Symbol] -> [[Symbol]] -> Symbol
findSymbol2 name1 [] rest = findSymbol name1 rest
findSymbol2 name1 (x:xs) rest | name x == name1 = x
findSymbol2 name1 (x:xs) rest | otherwise = findSymbol2 name1 xs rest

findValue :: String -> [[Symbol]] -> [[Symbol]] -> Value
findValue name1 [] symbols = (ValueError (Error 80))
findValue name1 ([]:xs) symbols = findValue name1 xs symbols
findValue name1 (x:xs) symbols = findValue2 name1 x xs symbols

findValue2 :: String -> [Symbol] -> [[Symbol]] -> [[Symbol]] -> Value
findValue2 name1 [] rest symbols = findValue name1 rest symbols
findValue2 name1 (x:xs) rest symbols | name x == name1 = (fst (evaluateExpression (rep x) 0 symbols))
findValue2 name1 (x:xs) rest symbols | otherwise = findValue2 name1 xs rest symbols

evaluateValue :: Value -> String -> [[Symbol]] -> Value
evaluateValue (ValueInt int) _ symbols = (ValueInt int)
evaluateValue (ValueBool bool) _ symbols = (ValueBool bool)
evaluateValue (ValueError (Error 1)) name1 symbols = findValue name1 symbols symbols
evaluateValue val _ _ = val

evaluateCondition :: (Value, [[Symbol]]) -> Expression -> Expression -> Int -> (Value, [[Symbol]])
evaluateCondition ((ValueBool True), symbols) exp2 exp3 recursion = (evaluateExpression exp2 (recursion + 1) symbols)
evaluateCondition ((ValueBool False), symbols) exp2 exp3 recursion = (evaluateExpression exp3 (recursion + 1) symbols)
evaluateCondition ((ValueError (Error 80)), symbols) _ _ _ = ((ValueError (Error 80)), symbols)
evaluateCondition ((ValueError (Error 81)), symbols) _ _ _ = ((ValueError (Error 81)), symbols)
evaluateCondition ((ValueError (Error 83)), symbols) _ _ _ = ((ValueError (Error 83)), symbols)
evaluateCondition (val, symbols) _ _ _ = (ValueError (Error 82), symbols)

evaluateEqual :: (Value, [[Symbol]]) -> (Value, [[Symbol]]) -> (Value, [[Symbol]])
evaluateEqual ((ValueError err), symbols) _ = ((ValueError err), symbols)
evaluateEqual _ ((ValueError err), symbols) = ((ValueError err), symbols)
evaluateEqual ((ValueInt val1), symbols) ((ValueInt val2), _)
                | val1 == val2 = ((ValueBool True), symbols)
                | otherwise = ((ValueBool False), symbols)
evaluateEqual ((ValueBool val1), symbols) ((ValueBool val2), _)
                | val1 == val2 = ((ValueBool True), symbols)
                | otherwise = ((ValueBool False), symbols)
evaluateEqual (_, symbols) _ = ((ValueError (Error 82)), symbols)

evaluateSmaller :: (Value, [[Symbol]]) -> (Value, [[Symbol]]) -> (Value, [[Symbol]])
evaluateSmaller ((ValueError err), symbols) _ = ((ValueError err), symbols)
evaluateSmaller _ ((ValueError err), symbols) = ((ValueError err), symbols)
evaluateSmaller ((ValueInt val1), symbols) ((ValueInt val2), _)
                | val1 < val2 = ((ValueBool True), symbols)
                | otherwise = ((ValueBool False), symbols)
evaluateSmaller (_, symbols) _ = ((ValueError (Error 82)), symbols)

evaluateLambda :: [[Symbol]] -> [Expression] -> Int -> (Value, [[Symbol]])
evaluateLambda symbols _ 100 = ((ValueError (Error 81)), symbols)
evaluateLambda symbols (x:[]) recursion = evaluateExpression x (recursion + 1) symbols
evaluateLambda symbols (x:xs) recursion = evaluateLambda (snd (evaluateExpression x recursion symbols)) xs (recursion + 1)

addArgs :: [[Symbol]] -> [Symbol] -> [Symbol] -> [[Symbol]]
addArgs symbols [] [] = symbols
addArgs (x:xs) (y:ys) (z:zs) = addArgs (((Symbol (name y) (rep z) ) : x) : xs) ys zs

evaluateSymbol :: Symbol -> [Symbol] -> [[Symbol]] -> Int -> (Value, [[Symbol]])
evaluateSymbol (Symbol name1 (Value (ValueError (Error err)) _)) _ symbols _ = ((ValueError (Error err)), symbols)
evaluateSymbol (Symbol name1 (Lambda args1 expressions)) args2 symbols recursion  | length args1 == length args2 = evaluateLambda (addArgs symbols args1 args2) expressions recursion
                                                                            | otherwise = ((ValueError (Error 85)), symbols)
evaluateSymbol (Symbol name1 exp1) [] symbols recursion = (evaluateExpression exp1 (recursion + 1) symbols)
evaluateSymbol (Symbol name1 exp1) args symbols recursion = ((ValueError (Error 85)), symbols)

removeLastArray :: (Value, [[Symbol]]) -> (Value, [[Symbol]])
removeLastArray (val, (x:xs)) = (val, xs)

evaluateExpression :: Expression -> Int -> [[Symbol]] -> (Value, [[Symbol]])
evaluateExpression _ 100 symbols = ((ValueError (Error 81)), symbols)
evaluateExpression (Value val name1) recursion symbols = ((evaluateValue val name1 symbols) , symbols)
evaluateExpression (Plus exp1 exp2) recursion symbols = removeLastArray (evaluatePlus (evaluateExpression exp1 (recursion + 1) ([] : symbols)) (evaluateExpression exp2 (recursion + 1) ([] : symbols)))
evaluateExpression (Minus exp1 exp2) recursion symbols = removeLastArray (evaluateMinus (evaluateExpression exp1 (recursion + 1) ([] : symbols)) (evaluateExpression exp2 (recursion + 1) ([] : symbols)))
evaluateExpression (Times exp1 exp2) recursion symbols = removeLastArray (evaluateTimes (evaluateExpression exp1 (recursion + 1) ([] : symbols)) (evaluateExpression exp2 (recursion + 1) ([] : symbols)))
evaluateExpression (Divided exp1 exp2) recursion symbols = removeLastArray (evaluateDivided (evaluateExpression exp1 (recursion + 1) ([] : symbols)) (evaluateExpression exp2 (recursion + 1) ([] : symbols)))
evaluateExpression (Modulo exp1 exp2) recursion symbols = removeLastArray (evaluateModulo (evaluateExpression exp1 (recursion + 1) ([] : symbols)) (evaluateExpression exp2 (recursion + 1) ([] : symbols)))
evaluateExpression (Define name1 exp1) recursion (x:xs) = ((ValueError (Error 0)), (((Symbol name1 exp1):x) : xs))
evaluateExpression (Condition exp1 exp2 exp3) recursion symbols = removeLastArray (evaluateCondition (evaluateExpression exp1 (recursion + 1) ([] : symbols)) exp2 exp3 recursion)
evaluateExpression (Equal exp1 exp2) recursion symbols = removeLastArray (evaluateEqual (evaluateExpression exp1 (recursion + 1) ([] : symbols)) (evaluateExpression exp2 (recursion + 1) ([] : symbols)))
evaluateExpression (Smaller exp1 exp2) recursion symbols = removeLastArray (evaluateSmaller (evaluateExpression exp1 (recursion + 1) ([] : symbols)) (evaluateExpression exp2 (recursion + 1) ([] : symbols)))
evaluateExpression (Lambda args expressions) recursion (x:xs) = removeLastArray (evaluateLambda ([]:(args ++ x):xs) expressions recursion)
evaluateExpression (SymbolExpression name1 args) recursion symbols = removeLastArray (evaluateSymbol (findSymbol name1 symbols) args ([] : symbols) (recursion + 1))
