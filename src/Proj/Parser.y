{
module Proj.Parser (
  parseExpr,
) where

import Control.Monad.Except
import Proj.Lexer
}

%name expr
%tokentype { Token }
--%monad { Except String } { (>>=) } { return }
%error { parseError }

%token
  var     { TokName $$ }
  sym     { TokSym  $$ }
  str     { TokStr  $$ }
  num     { TokNum  $$ }
  verb    { TokVerb $$ }
  averb   { TokAdverb $$ }
  ':'	  { TokAssign }
  '('	  { TokLParen }
  ')'	  { TokRParen }
  '{'	  { TokLBrace }
  '}'	  { TokRBrace }
  '['	  { TokLBrack }
  ']'	  { TokRBrack }

-- Grammar is E comprising expression, noun, function, term with words Noun, Verb, Adverb 
%%

Exprs : Expr                   {$1}
      | Exprs Expr             {App $1 $2}

Expr  : Exprs                         {$1}
      | '{' '[' Expr ']' Expr '}'     { Fun $3 $5} 
      | Expr verb Expr                { Op (opCode $2) $1 $3 }
      | '(' Expr ')'                  { $2 }
      | num                           { Lit (LInteger $1) }

{

data Literal
	= LBool String
	| LInteger String
	| LFloat String
	| LString String
    | LSymbol String

data Verb
  = Add
  | Sub
  | Mul
  | Div

data Expr 
	= Var String
    | Op Verb Expr Expr
    | App Expr Expr
	| List [Expr]
    | Lit Literal
    | Fun Expr Expr
    -- | Lambda IFunc EnvContext

-- data IFunc = IFunc [Expr] [Expr]-- IFunc { fn :: [Expr] -> Eval Expr}

opCode :: String -> Verb
opCode "+" = Add
opCode "-" = Sub
opCode "%" = Div
opCode "*" = Mul

parseExpr :: String -> Expr
parseExpr = expr . scanTokens

parseError :: [Token] -> a
parseError _ = error "ParseError!"

}
