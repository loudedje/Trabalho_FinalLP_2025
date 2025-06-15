# Interpretador Funcional com Suporte a Listas - Linguagens de Programação

Este projeto é um interpretador de uma linguagem funcional, desenvolvido em **Haskell** da disciplina de **Linguagens de Programação**.

## ✨ Funcionalidades Implementadas

- ✅ Aritmética com inteiros: `+`, `-`, `*`
- ✅ Booleanos: `true`, `false`, `and`, `or`, `not`, `if-then-else`
- ✅ Suporte a listas:
  - Construção com `cons` e `nil`
  - Funções: `head`, `tail`, `isNil`, `length`
  - Operações com listas (ex: soma de listas, soma com número)
- ✅ Tipos:
  - `TNum`, `TBool`, `TList`, `TFun`
- ✅ Verificador de tipos (`typecheck`)
- ✅ Avaliação com `step` (um passo) e `eval` (completo)


  🔗 **[Clique aqui para assistir à demonstração](https://seu-link-aqui.com)**


---

## 📁 Estrutura dos Arquivos

- `Lexer.hs`: analisador léxico e definição dos tipos
- `Parser.y`: analisador sintático com Happy
- `Interpreter.hs`: definição da semântica operacional (`step`, `eval`)
- `TypeChecker.hs`: verificação de tipos

---

## 📌 Exemplos de uso no GHCi

```haskell
-- Soma normal
eval (Add (Num 15) (Num (-7))) 
-- Resultado: Num 8

-- Lista com cons
eval (Cons (Num 1) (List [Num 2, Num 3]))
-- Resultado: List [Num 1, Num 2, Num 3]

-- Cabeça da lista
eval (App (Var "head") (List [Num 4, Num 5])) 
-- Resultado: Num 4

-- Verificador de tipo
typecheck (Add (Num 2) (Num 3)) 
-- Resultado: expressão sem erro de tipo
