{-
  A basic calculator for arithmetic expressions
  Based on the example in Chapter 8 of "Programming in Haskell"
  by Graham Hutton.

  Pedro Vasconcelos, 2025
-}
module Main where

import Parsing
import Data.Char
import qualified Data.Map.Strict as Map

--
-- a data type for expressions
-- made up from integer numbers, + and *
--
type Name = String
type Env = Map.Map Name Integer

data Expr = Num Integer
          | Add Expr Expr
          | Mul Expr Expr
          | Sub Expr Expr
          | Div Expr Expr
          | Mod Expr Expr
          | Var Name
          deriving Show

data Command = Expression Expr
             | Assign Name Expr
             deriving Show

-- a recursive evaluator for expressions
--
eval :: Expr -> Env -> Integer
eval (Num n) env = n
eval (Var v) env = case Map.lookup v env of
                        Just b -> b
                        Nothing -> error ("undefined variable" ++ v)
eval (Add e1 e2) env = eval e1 env + eval e2 env
eval (Mul e1 e2) env = eval e1 env * eval e2 env
eval (Sub e1 e2) env = eval e1 env - eval e2 env
eval (Div e1 e2) env = eval e1 env `div` eval e2 env
eval (Mod e1 e2) env = eval e1 env `mod` eval e2 env

evalCommand :: Command -> Env -> (Integer, Env)
evalCommand (Expression e) env = (eval e env, env)
evalCommand (Assign v e) env = let res = eval e env in (res, Map.insert v res env )

-- | a parser for expressions
-- Grammar rules:
--
-- expr ::= term exprCont
-- exprCont ::= '+' term exprCont | epsilon

-- term ::= factor termCont
-- termCont ::= '*' factor termCont | epsilon

-- factor ::= natural | '(' expr ')'

expr :: Parser Expr
expr = do t <- term
          exprCont t

exprCont :: Expr -> Parser Expr
exprCont acc = do char '+'
                  t <- term
                  exprCont (Add acc t)
               <|> 
               do char '-'
                  t <- term
                  exprCont (Sub acc t)
               <|> return acc
              
term :: Parser Expr
term = do f <- factor
          termCont f

termCont :: Expr -> Parser Expr
termCont acc =  do char '*'
                   f <- factor  
                   termCont (Mul acc f)
                 <|> 
                 do char '/'
                    f <- factor
                    termCont (Div acc f)
                 <|>
                 do char '%'
                    f <- factor  
                    termCont (Mod acc f) 
                 <|> return acc

factor :: Parser Expr
factor = do n <- natural
            return (Num n)
          <|>
          do n <- name
             return (Var n)
          <|>
          do char '('
             e <- expr
             char ')'
             return e

command :: Parser Command
command = do n <- name
             char '='
             e <- expr
             return (Assign n e)
          <|>
          do e <- expr
             return (Expression e)

natural :: Parser Integer
natural = do xs <- many1 (satisfy isDigit)
             return (read xs)

name :: Parser Name
name = do xs <- many1 (satisfy isAlpha)
          return xs
----------------------------------------------------------------             
  
main :: IO ()
main
  = do txt <- getContents
       calculator (lines txt) Map.empty 

-- | read-eval-print loop
calculator :: [String] -> Env -> IO ()
calculator [] _ = return ()
calculator (l:ls) env = do 
                          putStrLn res
                          calculator ls  envNew
                        where (res, envNew) = evaluate env l 

-- | evaluate a single expression
evaluate :: Env -> String -> (String, Env)
evaluate env txt = case parse command txt of
      [ (tree, "") ] -> let (res, envNew) = evalCommand tree env in (show res, envNew)
      _ -> ("parse error; try again", env)
      