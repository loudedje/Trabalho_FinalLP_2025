module Lexer where

import Data.Char

data Expr = BTrue 
          | BFalse 
          | Num Int 
          | Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | And Expr Expr 
          | Or Expr Expr
          | Cons Expr Expr
          | Nil
          | IsNil Expr
          | Head Expr
          | Tail Expr
          | If Expr Expr Expr 
          | Var String 
          | Lam String Ty Expr 
          | App Expr Expr 
          | Paren Expr 
          | List [Expr]  
          | Length Expr
          | Equal Expr Expr  
          | NotEqual Expr Expr 
          | Not Expr
          deriving Show 

data Ty = TBool 
        | TNum 
        | TFun Ty Ty 
        | TList Ty
        deriving (Show, Eq)

data Token = TokenTrue 
           | TokenFalse 
           | TokenNum Int 
           | TokenAdd 
           | TokenSub
           | TokenMul
           | TokenAnd 
           | TokenOr
           | TokenIf 
           | TokenThen
           | TokenElse 
           | TokenVar String 
           | TokenLam 
           | TokenColon
           | TokenArrow 
           | TokenTNum 
           | TokenTBool
           | TokenLParen 
           | TokenRParen 
           | TokenCons
           | TokenNil
           | TokenIsNil
           | TokenHead
           | TokenTail
           | TokenCrochetE
           | TokenCrochetD
           | TokenVirgula
           | TokenLength
           | TokenEqual
           | TokenNotEqual
           | TokenNot
           deriving Show 

lexer :: String -> [Token]
lexer [] = [] 
lexer ('\\':cs) = TokenLam : lexer cs 
lexer (':':cs) = TokenColon : lexer cs 
lexer ('(':cs) = TokenLParen : lexer cs 
lexer (')':cs) = TokenRParen : lexer cs 
lexer ('&':'&':cs) = TokenAnd : lexer cs 
lexer ('-':'>':cs) = TokenArrow : lexer cs 
lexer ('+':cs) = TokenAdd : lexer cs  -- Operador de adição
lexer ('|':'|':cs) = TokenOr : lexer cs  -- Operador lógico OR
lexer ('*':cs) = TokenMul : lexer cs  -- Operador de multiplicação
lexer ('=':'=':cs) = TokenEqual : lexer cs  -- Operador de igualdade (==)
lexer ('!':'=':cs) = TokenNotEqual: lexer cs  -- Operador de desigualdade (!=)
lexer ('!':cs) = TokenNot : lexer cs  -- Operador NOT
lexer ('[':cs) = TokenCrochetE : lexer cs  -- Abertura de lista
lexer (']':cs) = TokenCrochetD : lexer cs  -- Fechamento de lista
lexer (',':cs) = TokenVirgula : lexer cs  -- Separador de lista
lexer (c:cs) | isSpace c = lexer cs 
             | isDigit c = lexNum (c:cs) 
             | isAlpha c = lexKW (c:cs)
             | otherwise = error ("Invalid character: " ++ [c])  -- Erro para caracteres desconhecidos

lexNum :: String -> [Token]
lexNum cs = case span isDigit cs of 
              (num, rest) -> TokenNum (read num) : lexer rest 

lexKW :: String -> [Token]
lexKW cs = case span isAlpha cs of 
             ("true", rest) -> TokenTrue : lexer rest 
             ("false", rest) -> TokenFalse : lexer rest 
             ("if", rest) -> TokenIf : lexer rest 
             ("then", rest) -> TokenThen : lexer rest 
             ("else", rest) -> TokenElse : lexer rest 
             ("Number", rest) -> TokenTNum : lexer rest 
             ("Boolean", rest) -> TokenTBool : lexer rest 
             ("cons", rest) -> TokenCons : lexer rest 
             ("nil", rest) -> TokenNil : lexer rest 
             ("isNil", rest) -> TokenIsNil : lexer rest 
             ("head", rest) -> TokenHead : lexer rest 
             ("tail", rest) -> TokenTail : lexer rest
             ("length", rest) -> TokenLength : lexer rest  -- Corrigido para TokenLength
             (var, rest) -> TokenVar var : lexer rest
