{module Parser where

import Lexer

}
%name parser
%tokentype { Token }
%error { parseError }

%token
    num             { TokenNum $$ }
    true            { TokenTrue }
    false           { TokenFalse }
    '+'             { TokenAdd }
    '-'             { TokenSub }
    '*'             { TokenMul }
    "&&"            { TokenAnd }
    "||"            { TokenOr }
    "=="            { TokenEq }
    "!="            { TokenNeq }
    if              { TokenIf }
    then            { TokenThen }
    else            { TokenElse }
    var             { TokenVar $$ }
    '\\'            { TokenLam }
    ':'             { TokenColon }
    "->"            { TokenArrow }
    Number          { TokenTNum }
    Boolean         { TokenTBool }
    '('             { TokenLParen }
    ')'             { TokenRParen }
    '['             { TokenCrochetE }
    ']'             { TokenCrochetD }
    ','             { TokenVirgula }
    "length"        { TokenLength }
    "not"           { TokenNot }

%nonassoc if then else
%nonassoc '\\'
%left '+' '-'
%left '*' 
%left "&&" "||"

%%

Exp     : if Exp then Exp else Exp      { If $2 $4 $6 }
        | true                          { BTrue }
        | false                         { BFalse }
        | Exp '+' Exp                   { Add $1 $3 }
        | Exp '-' Exp                   { Sub $1 $3 }
        | Exp '*' Exp                   { Mul $1 $3 }
        | Exp "&&" Exp                  { And $1 $3 }
        | Exp "||" Exp                  { Or $1 $3 }
        | Exp "==" Exp                  { Equal $1 $3 }
        | Exp "!=" Exp                  { NotEqual $1 $3 }
        | "not" Exp                     { Not $2 }
        | var                           { Var $1 }
        | num                            { Num $1 }
        | '\\' var ':' Type "->" Exp    { Lam $2 $4 $6 }
        | Exp Exp                       { App $1 $2 }
        | '(' Exp ')'                   { Paren $2 }
        | '[' ListElems ']'             { List $2 }
        | "length" Exp                  { Length $2 }  -- Corrigido de "lenght" para 

Type    : Boolean                       { TBool }
        | Number                        { TNum }
        | '(' Type "->" Type ')'        { TFun $2 $4 }

ListElems : Exp                        { [$1] }
          | Exp ',' ListElems         { $1 : $3 }

{ 
parseError :: [Token] -> a 
parseError _ = error "Erro sintático!"  -- Define o tratamento de erro
}
