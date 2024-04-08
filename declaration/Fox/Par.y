-- -*- haskell -*- File generated by the BNF Converter (bnfc 2.9.5).

-- Parser definition for use with Happy
{
{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-warn-overlapping-patterns #-}
{-# LANGUAGE PatternSynonyms #-}

module Fox.Par
  ( happyError
  , myLexer
  , pProgram
  ) where

import Prelude

import qualified Fox.Abs
import Fox.Lex

}

%name pProgram Program
-- no lexer declaration
%monad { Err } { (>>=) } { return }
%tokentype {Token}
%token
  '!'       { PT _ (TS _ 1)  }
  '!='      { PT _ (TS _ 2)  }
  '%'       { PT _ (TS _ 3)  }
  '&&'      { PT _ (TS _ 4)  }
  '('       { PT _ (TS _ 5)  }
  ')'       { PT _ (TS _ 6)  }
  '*'       { PT _ (TS _ 7)  }
  '+'       { PT _ (TS _ 8)  }
  ','       { PT _ (TS _ 9)  }
  '-'       { PT _ (TS _ 10) }
  '/'       { PT _ (TS _ 11) }
  ';'       { PT _ (TS _ 12) }
  '<'       { PT _ (TS _ 13) }
  '<='      { PT _ (TS _ 14) }
  '='       { PT _ (TS _ 15) }
  '=='      { PT _ (TS _ 16) }
  '>'       { PT _ (TS _ 17) }
  '>='      { PT _ (TS _ 18) }
  'boolean' { PT _ (TS _ 19) }
  'else'    { PT _ (TS _ 20) }
  'false'   { PT _ (TS _ 21) }
  'if'      { PT _ (TS _ 22) }
  'int'     { PT _ (TS _ 23) }
  'return'  { PT _ (TS _ 24) }
  'string'  { PT _ (TS _ 25) }
  'true'    { PT _ (TS _ 26) }
  'var'     { PT _ (TS _ 27) }
  'while'   { PT _ (TS _ 28) }
  '{'       { PT _ (TS _ 29) }
  '||'      { PT _ (TS _ 30) }
  '}'       { PT _ (TS _ 31) }
  L_Ident   { PT _ (TV $$)   }
  L_integ   { PT _ (TI $$)   }
  L_quoted  { PT _ (TL $$)   }

%%

Ident :: { Fox.Abs.Ident }
Ident  : L_Ident { Fox.Abs.Ident $1 }

Integer :: { Integer }
Integer  : L_integ  { (read $1) :: Integer }

String  :: { String }
String   : L_quoted { $1 }

Program :: { Fox.Abs.Program }
Program : ListFun { Fox.Abs.Prog $1 }

Fun :: { Fox.Abs.Fun }
Fun
  : Type Ident '(' ListArg ')' Block { Fox.Abs.FunDef $1 $2 $4 $6 }

ListFun :: { [Fox.Abs.Fun] }
ListFun : Fun { (:[]) $1 } | Fun ListFun { (:) $1 $2 }

Arg :: { Fox.Abs.Arg }
Arg
  : 'var' Type Ident { Fox.Abs.ArgVar $2 $3 }
  | Type Ident { Fox.Abs.ArgVal $1 $2 }

ListArg :: { [Fox.Abs.Arg] }
ListArg
  : {- empty -} { [] }
  | Arg { (:[]) $1 }
  | Arg ',' ListArg { (:) $1 $3 }

Block :: { Fox.Abs.Block }
Block : '{' ListStmt '}' { Fox.Abs.Bl $2 }

ListStmt :: { [Fox.Abs.Stmt] }
ListStmt : {- empty -} { [] } | Stmt ListStmt { (:) $1 $2 }

Stmt :: { Fox.Abs.Stmt }
Stmt
  : Block { Fox.Abs.BlockStmt $1 }
  | Type ListItem ';' { Fox.Abs.Decl $1 $2 }
  | Ident '=' Expr ';' { Fox.Abs.Ass $1 $3 }
  | 'return' Expr ';' { Fox.Abs.Ret $2 }
  | 'if' '(' Expr ')' Stmt { Fox.Abs.If $3 $5 }
  | 'if' '(' Expr ')' Stmt 'else' Stmt { Fox.Abs.IfElse $3 $5 $7 }
  | 'while' '(' Expr ')' Stmt { Fox.Abs.While $3 $5 }
  | Expr ';' { Fox.Abs.SExp $1 }
  | Fun { Fox.Abs.FSt $1 }

Item :: { Fox.Abs.Item }
Item
  : Ident { Fox.Abs.NoInit $1 }
  | Ident '=' Expr { Fox.Abs.Init $1 $3 }

ListItem :: { [Fox.Abs.Item] }
ListItem : Item { (:[]) $1 } | Item ',' ListItem { (:) $1 $3 }

Type :: { Fox.Abs.Type }
Type
  : 'int' { Fox.Abs.Int }
  | 'string' { Fox.Abs.Str }
  | 'boolean' { Fox.Abs.Bool }

Expr6 :: { Fox.Abs.Expr }
Expr6
  : Ident { Fox.Abs.EVar $1 }
  | Integer { Fox.Abs.ELitInt $1 }
  | 'true' { Fox.Abs.ELitTrue }
  | 'false' { Fox.Abs.ELitFalse }
  | Ident '(' ListExpr ')' { Fox.Abs.EApp $1 $3 }
  | String { Fox.Abs.EString $1 }
  | '(' Expr ')' { $2 }

Expr5 :: { Fox.Abs.Expr }
Expr5
  : '-' Expr6 { Fox.Abs.Neg $2 }
  | '!' Expr6 { Fox.Abs.Not $2 }
  | Expr6 { $1 }

Expr4 :: { Fox.Abs.Expr }
Expr4 : Expr4 MulOp Expr5 { Fox.Abs.EMul $1 $2 $3 } | Expr5 { $1 }

Expr3 :: { Fox.Abs.Expr }
Expr3 : Expr3 AddOp Expr4 { Fox.Abs.EAdd $1 $2 $3 } | Expr4 { $1 }

Expr2 :: { Fox.Abs.Expr }
Expr2 : Expr2 RelOp Expr3 { Fox.Abs.ERel $1 $2 $3 } | Expr3 { $1 }

Expr1 :: { Fox.Abs.Expr }
Expr1 : Expr2 '&&' Expr1 { Fox.Abs.EAnd $1 $3 } | Expr2 { $1 }

Expr :: { Fox.Abs.Expr }
Expr : Expr1 '||' Expr { Fox.Abs.EOr $1 $3 } | Expr1 { $1 }

ListExpr :: { [Fox.Abs.Expr] }
ListExpr
  : {- empty -} { [] }
  | Expr { (:[]) $1 }
  | Expr ',' ListExpr { (:) $1 $3 }

AddOp :: { Fox.Abs.AddOp }
AddOp : '+' { Fox.Abs.Plus } | '-' { Fox.Abs.Minus }

MulOp :: { Fox.Abs.MulOp }
MulOp
  : '*' { Fox.Abs.Times } | '/' { Fox.Abs.Div } | '%' { Fox.Abs.Mod }

RelOp :: { Fox.Abs.RelOp }
RelOp
  : '<' { Fox.Abs.LTH }
  | '<=' { Fox.Abs.LE }
  | '>' { Fox.Abs.GTH }
  | '>=' { Fox.Abs.GE }
  | '==' { Fox.Abs.EQU }
  | '!=' { Fox.Abs.NE }

{

type Err = Either String

happyError :: [Token] -> Err a
happyError ts = Left $
  "syntax error at " ++ tokenPos ts ++
  case ts of
    []      -> []
    [Err _] -> " due to lexer error"
    t:_     -> " before `" ++ (prToken t) ++ "'"

myLexer :: String -> [Token]
myLexer = tokens

}
