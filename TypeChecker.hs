module TypeChecker (typecheck, typeof) where

import Lexer

type Ctx = [(String, Ty)]

-- Função para verificar o tipo de uma expressão
typeof :: Ctx -> Expr -> Maybe Ty
typeof ctx (Num _) = Just TNum          -- Números são do tipo TNum
typeof ctx BFalse = Just TBool            -- Booleanos são do tipo TBool
typeof ctx BTrue = Just TBool
typeof ctx (Add e1 e2) = 
  case (typeof ctx e1, typeof ctx e2) of
    (Just TNum, Just TNum) -> Just TNum  -- A operação de adição só funciona com TNum
    _ -> Nothing

typeof ctx (Sub e1 e2) = 
  case (typeof ctx e1, typeof ctx e2) of
    (Just TNum, Just TNum) -> Just TNum  -- Subtração funciona com TNum
    _ -> Nothing

typeof ctx (Mul e1 e2) = 
  case (typeof ctx e1, typeof ctx e2) of
    (Just TNum, Just TNum) -> Just TNum  -- Multiplicação funciona com TNum
    _ -> Nothing

--Lista

typeof ctx (Length lst) = 
  case typeof ctx lst of
    Just (TList _) -> Just TNum  -- length só funciona com listas e retorna um número
    _ -> Nothing

typeof ctx (Cons e1 e2) = 
  case (typeof ctx e1, typeof ctx e2) of
    (Just t1, Just (TList t2)) | t1 == t2 -> Just (TList t1)  -- Cons adiciona um elemento na lista
    _ -> Nothing

typeof ctx (List xs) = 
  case map (typeof ctx) xs of
    (Just t:ts) | all (== Just t) ts -> Just (TList t)  -- Se todos os elementos forem do mesmo tipo, é uma lista do tipo TList
    _ -> Nothing

typeof ctx (IsNil lst) = 
  case typeof ctx lst of
    Just (TList _) -> Just TBool  -- IsNil retorna um valor booleano
    _ -> Nothing

typeof ctx (Head lst) = 
  case typeof ctx lst of
    Just (TList t) -> Just t  -- Head retorna o tipo dos elementos da lista
    _ -> Nothing

typeof ctx (Tail lst) = 
  case typeof ctx lst of
    Just (TList t) -> Just (TList t)  -- Tail retorna uma lista do mesmo tipo
    _ -> Nothing

--Operadores lógicos
typeof ctx (And e1 e2) = 
    case (typeof ctx e1, typeof ctx e2) of
        (Just TBool, Just TBool) -> Just TBool
        _ -> Nothing

typeof ctx (Or e1 e2) = 
    case (typeof ctx e1, typeof ctx e2) of
        (Just TBool, Just TBool) -> Just TBool
        _ -> Nothing

typeof ctx (Not e) = 
    case typeof ctx e of
        Just TBool -> Just TBool
        _ -> Nothing

--Operadores relacionais
typeof ctx (Equal e1 e2) = 
    case (typeof ctx e1, typeof ctx e2) of
        (Just TNum, Just TNum) -> Just TBool  -- A comparação de números retorna um booleano
        _ -> Nothing

typeof ctx (NotEqual e1 e2) = 
    case (typeof ctx e1, typeof ctx e2) of
        (Just TNum, Just TNum) -> Just TBool
        _ -> Nothing


typeof ctx (If e1 e2 e3) = 
  case (typeof ctx e1) of
    Just TBool -> case (typeof ctx e2, typeof ctx e3) of
                      (Just t1, Just t2) | t1 == t2  -> Just t1
                                         | otherwise -> Nothing
                      _ -> Nothing
    _ -> Nothing



typeof ctx (Var v) = lookup v ctx  -- O tipo da variável é procurado no contexto

typeof ctx (Lam x t1 b) = let ctx' = (x, t1) : ctx in 
                            case typeof ctx' b of
                              Just t2 -> Just (TFun t1 t2)  -- Função com tipo TFun
                              _       -> Nothing

typeof ctx (App e1 e2) = 
  case (typeof ctx e1) of
    Just (TFun t11 t12) -> case typeof ctx e2 of
                             Just t2 -> if t11 == t2 then 
                                          Just t12 
                                        else 
                                          Nothing
                             _ -> Nothing
    _ -> Nothing

typeof ctx (Paren e) = typeof ctx e  -- Parênteses não alteram o tipo

-- Função de verificação de tipo
typecheck :: Expr -> Expr  
typecheck e = case typeof [] e of 
                Just _ -> e  -- Se o tipo for válido, retorna a expressão
                Nothing -> error "Erro de tipo!"  -- Se o tipo for inválido, lança erro
