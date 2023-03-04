--
-- EPITECH PROJECT, 2023
-- GlaDos
-- File description:
-- Loop
--

module Loop (
    loop,
    inputLoop
    ) where

import AST
import Translator
-- import Evaluate
import EvaluateStack
import Parser
import System.IO (isEOF)

ifValueIsInt :: Value -> Bool
ifValueIsInt (ValueInt _) = True
ifValueIsInt _ = False

ifValueIsBool :: Value -> Bool
ifValueIsBool (ValueBool _) = True
ifValueIsBool _ = False

ifValueIsError :: Value -> Bool
ifValueIsError (ValueError _) = True
ifValueIsError _ = False

handleValue :: Value -> Int
handleValue value =
  if ifValueIsInt value
    then 1
    else if ifValueIsBool value
           then 2
           else if ifValueIsError value
                  then 3
                  else 4

singleExpression :: [String] -> [[Symbol]] -> Int -> (Value, [String], [[Symbol]])
singleExpression x env c = let  res = getNext x [] 0
                                exp = evaluateExpression (createExpression (fst res)) c env
    in
        ((fst exp), (snd res), (snd exp))

loop :: [String] -> [[Symbol]] -> Int -> IO ()
loop [] _ _ = putStr ""
loop input env c = do 
    let (res, rest, newEnv) = (singleExpression input env c)
    let t = getNext input [] 0
    case res of
        ValueError (Error code) -> if code < 80
                                    then putStr ""
                                    else do
                                        print res
        ValueInt (value) -> do
                                print value
        ValueBool (value) ->    if value == True
                                    then putStr "#t"
                                    else do 
                                        putStr "#f"
    loop rest (newEnv) c

activeLoop :: [String] -> [[Symbol]] -> (Value, [[Symbol]])
activeLoop [] _ = ((ValueError (Error 86)), ([[]]))
activeLoop input env = let (v, r, e) = singleExpression input env 0
                in
                    (v, e)

inputLoop :: [[Symbol]] -> IO ()
inputLoop env = do
    isEnd <- isEOF
    if isEnd
        then return ()
        else do
            line <- getLine
            let input = parser line []
            let (val, e) = activeLoop input env
            case val of
                ValueError (Error code) -> if code < 80
                                            then putStr ""
                                            else do
                                                print val
                ValueInt (value) -> do
                                        print value
                ValueBool (value) ->    if (value == True)
                                            then print "#t"
                                            else do
                                                print "#f"
            inputLoop (e)