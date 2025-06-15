module Interpreter(eval, step) where 

import Lexer 

isValue :: Expr -> Bool 
isValue BTrue       = True 
isValue BFalse      = True  
isValue (Num _)     = True 
isValue (Lam _ _ _) = True
isValue (List xs)   = all isValue xs 
isValue _           = False 


-- Substituição de variáveis
subst :: String -> Expr -> Expr -> Expr
subst v e BTrue = BTrue 
subst v e BFalse = BFalse 
subst v e (Num x) = Num x 
subst v e (Add e1 e2) = Add (subst v e e1) (subst v e e2)
subst v e (Sub e1 e2) = Sub (subst v e e1) (subst v e e2)
subst v e (Mul e1 e2) = Mul (subst v e e1) (subst v e e2)
subst v e (And e1 e2) = And (subst v e e1) (subst v e e2)
subst v e (Or e1 e2) = Or (subst v e e1) (subst v e e2)
subst v e (If e1 e2 e3) = If (subst v e e1) (subst v e e2) (subst v e e3)
subst v e (Var x) = if v == x then 
                      e 
                    else 
                      Var x 
subst v e (Lam x t b) = Lam x t (subst v e b)
subst v e (App e1 e2) = App (subst v e e1) (subst v e e2)
subst v e (Paren e1) = Paren (subst v e e1)
subst v e (List xs) = List (map (subst v e) xs)


--Funções para verificar se a lista é vazia,cabeça e fim
step (IsNil (List xs))
  | null xs   = BTrue
  | otherwise = BFalse
step (App (Var "head") (List [])) = error "Erro: lista vazia não tem cabeça."
step (App (Var "head") (List (x:_))) = x
step (App (Var "tail") (List [])) = error "Erro: lista vazia não tem fim."
step (App (Var "tail") (List (_:xs))) = List xs
step (Add (List xs) (List ys)) = List (xs ++ ys)
step (Length (List xs)) = Num (length xs)
step (Length e) = error "Erro: 'length' só pode ser usado em listas."
step (Cons e1 (List xs)) = List (e1 : xs)      
step (Cons e1 e2) = Cons (step e1) e2  


--Operação aritméticas
step (Add (Num n1) (Num n2)) = Num (n1 + n2)     -- S-Add
step (Add (Num n1) e2) = let e2' = step e2       -- S-Add2
                           in Add (Num n1) e2' 
step (Add e1 e2) = Add (step e1) e2              -- S-Add1
step (Sub (Num n1) (Num n2)) = Num (n1 - n2)     -- S-Sub
step (Sub (Num n1) e2) = let e2' = step e2       -- S-Sub2
                           in Sub (Num n1) e2'
step (Sub e1 e2) = Sub (step e1) e2              -- S-Sub1
step (Mul (Num n1) (Num n2)) = Num (n1 * n2)     -- S-Mul
step (Mul (Num n1) e2) = let e2' = step e2       -- S-Mul2
                           in Mul (Num n1) e2'
step (Mul e1 e2) = Mul (step e1) e2              -- S-Mul1

-- Operadores Relacionais
step (Equal (Num n1) (Num n2)) = if n1 == n2 then BTrue else BFalse 
step (NotEqual (Num n1) (Num n2)) = if n1 /= n2 then BTrue else BFalse
                

-- Operadores Lógicos
step (And BTrue e2) = e2 
step (And BFalse e2) = BFalse 
step (And e1 e2) = And (step e1) e2 
step (Or BTrue e2) = BTrue
step (Or BFalse e2) = e2
step (Or e1 e2) = Or (step e1) e2
step (Not BTrue) = BFalse
step (Not BFalse) = BTrue
step (Not e) = Not (step e)
-- Estruturas de Controle
step (If BTrue e1 e2) = e1 
step (If BFalse e1 e2) = e2
step (If e e1 e2) = If (step e) e1 e2 
step (App e1@(Lam x t b) e2) | isValue e2 = subst x e2 b
                             | otherwise  = App e1 (step e2)
step (App e1 e2) = App (step e1) e2 
step (Paren e) = e 

eval :: Expr -> Expr 
eval e | isValue e = e 
       | otherwise = eval (step e)

