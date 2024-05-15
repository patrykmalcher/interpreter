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

%name pProgram_internal Program
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
  L_Ident   { PT _ (TV _)    }
  L_integ   { PT _ (TI _)    }
  L_quoted  { PT _ (TL _)    }

%%

Ident :: { (Fox.Abs.BNFC'Position, Fox.Abs.Ident) }
Ident  : L_Ident { (uncurry Fox.Abs.BNFC'Position (tokenLineCol $1), Fox.Abs.Ident (tokenText $1)) }

Integer :: { (Fox.Abs.BNFC'Position, Integer) }
Integer  : L_integ  { (uncurry Fox.Abs.BNFC'Position (tokenLineCol $1), (read (tokenText $1)) :: Integer) }

String  :: { (Fox.Abs.BNFC'Position, String) }
String   : L_quoted { (uncurry Fox.Abs.BNFC'Position (tokenLineCol $1), ((\(PT _ (TL s)) -> s) $1)) }

Program :: { (Fox.Abs.BNFC'Position, Fox.Abs.Program) }
Program : ListStmt { (fst $1, Fox.Abs.Prog (fst $1) (snd $1)) }

Block :: { (Fox.Abs.BNFC'Position, Fox.Abs.Block) }
Block
  : '{' ListStmt '}' { (uncurry Fox.Abs.BNFC'Position (tokenLineCol $1), Fox.Abs.Bl (uncurry Fox.Abs.BNFC'Position (tokenLineCol $1)) (snd $2)) }

ListStmt :: { (Fox.Abs.BNFC'Position, [Fox.Abs.Stmt]) }
ListStmt
  : {- empty -} { (Fox.Abs.BNFC'NoPosition, []) }
  | Stmt ListStmt { (fst $1, (:) (snd $1) (snd $2)) }

Stmt :: { (Fox.Abs.BNFC'Position, Fox.Abs.Stmt) }
Stmt
  : Block { (fst $1, Fox.Abs.BlStmt (fst $1) (snd $1)) }
  | Type Ident '=' Expr ';' { (fst $1, Fox.Abs.Decl (fst $1) (snd $1) (snd $2) (snd $4)) }
  | Ident '=' Expr ';' { (fst $1, Fox.Abs.Ass (fst $1) (snd $1) (snd $3)) }
  | 'return' Expr ';' { (uncurry Fox.Abs.BNFC'Position (tokenLineCol $1), Fox.Abs.Ret (uncurry Fox.Abs.BNFC'Position (tokenLineCol $1)) (snd $2)) }
  | 'if' '(' Expr ')' Stmt { (uncurry Fox.Abs.BNFC'Position (tokenLineCol $1), Fox.Abs.If (uncurry Fox.Abs.BNFC'Position (tokenLineCol $1)) (snd $3) (snd $5)) }
  | 'if' '(' Expr ')' Stmt 'else' Stmt { (uncurry Fox.Abs.BNFC'Position (tokenLineCol $1), Fox.Abs.IfElse (uncurry Fox.Abs.BNFC'Position (tokenLineCol $1)) (snd $3) (snd $5) (snd $7)) }
  | 'while' '(' Expr ')' Stmt { (uncurry Fox.Abs.BNFC'Position (tokenLineCol $1), Fox.Abs.While (uncurry Fox.Abs.BNFC'Position (tokenLineCol $1)) (snd $3) (snd $5)) }
  | Expr ';' { (fst $1, Fox.Abs.SExp (fst $1) (snd $1)) }
  | Fun { (fst $1, Fox.Abs.FSt (fst $1) (snd $1)) }

Fun :: { (Fox.Abs.BNFC'Position, Fox.Abs.Fun) }
Fun
  : Type Ident '(' ListArg ')' Block { (fst $1, Fox.Abs.FunDef (fst $1) (snd $1) (snd $2) (snd $4) (snd $6)) }

Arg :: { (Fox.Abs.BNFC'Position, Fox.Abs.Arg) }
Arg
  : 'var' Type Ident { (uncurry Fox.Abs.BNFC'Position (tokenLineCol $1), Fox.Abs.ArgVar (uncurry Fox.Abs.BNFC'Position (tokenLineCol $1)) (snd $2) (snd $3)) }
  | Type Ident { (fst $1, Fox.Abs.ArgVal (fst $1) (snd $1) (snd $2)) }

ListArg :: { (Fox.Abs.BNFC'Position, [Fox.Abs.Arg]) }
ListArg
  : {- empty -} { (Fox.Abs.BNFC'NoPosition, []) }
  | Arg { (fst $1, (:[]) (snd $1)) }
  | Arg ',' ListArg { (fst $1, (:) (snd $1) (snd $3)) }

Type :: { (Fox.Abs.BNFC'Position, Fox.Abs.Type) }
Type
  : 'int' { (uncurry Fox.Abs.BNFC'Position (tokenLineCol $1), Fox.Abs.Int (uncurry Fox.Abs.BNFC'Position (tokenLineCol $1))) }
  | 'string' { (uncurry Fox.Abs.BNFC'Position (tokenLineCol $1), Fox.Abs.Str (uncurry Fox.Abs.BNFC'Position (tokenLineCol $1))) }
  | 'boolean' { (uncurry Fox.Abs.BNFC'Position (tokenLineCol $1), Fox.Abs.Bool (uncurry Fox.Abs.BNFC'Position (tokenLineCol $1))) }

Expr6 :: { (Fox.Abs.BNFC'Position, Fox.Abs.Expr) }
Expr6
  : Ident { (fst $1, Fox.Abs.EVar (fst $1) (snd $1)) }
  | Integer { (fst $1, Fox.Abs.ELitInt (fst $1) (snd $1)) }
  | 'true' { (uncurry Fox.Abs.BNFC'Position (tokenLineCol $1), Fox.Abs.ELitTrue (uncurry Fox.Abs.BNFC'Position (tokenLineCol $1))) }
  | 'false' { (uncurry Fox.Abs.BNFC'Position (tokenLineCol $1), Fox.Abs.ELitFalse (uncurry Fox.Abs.BNFC'Position (tokenLineCol $1))) }
  | Ident '(' ListExpr ')' { (fst $1, Fox.Abs.EApp (fst $1) (snd $1) (snd $3)) }
  | String { (fst $1, Fox.Abs.EString (fst $1) (snd $1)) }
  | '(' Expr ')' { (uncurry Fox.Abs.BNFC'Position (tokenLineCol $1), (snd $2)) }

Expr5 :: { (Fox.Abs.BNFC'Position, Fox.Abs.Expr) }
Expr5
  : '-' Expr6 { (uncurry Fox.Abs.BNFC'Position (tokenLineCol $1), Fox.Abs.Neg (uncurry Fox.Abs.BNFC'Position (tokenLineCol $1)) (snd $2)) }
  | '!' Expr6 { (uncurry Fox.Abs.BNFC'Position (tokenLineCol $1), Fox.Abs.Not (uncurry Fox.Abs.BNFC'Position (tokenLineCol $1)) (snd $2)) }
  | Expr6 { (fst $1, (snd $1)) }

Expr4 :: { (Fox.Abs.BNFC'Position, Fox.Abs.Expr) }
Expr4
  : Expr4 MulOp Expr5 { (fst $1, Fox.Abs.EMul (fst $1) (snd $1) (snd $2) (snd $3)) }
  | Expr5 { (fst $1, (snd $1)) }

Expr3 :: { (Fox.Abs.BNFC'Position, Fox.Abs.Expr) }
Expr3
  : Expr3 AddOp Expr4 { (fst $1, Fox.Abs.EAdd (fst $1) (snd $1) (snd $2) (snd $3)) }
  | Expr4 { (fst $1, (snd $1)) }

Expr2 :: { (Fox.Abs.BNFC'Position, Fox.Abs.Expr) }
Expr2
  : Expr2 RelOp Expr3 { (fst $1, Fox.Abs.ERel (fst $1) (snd $1) (snd $2) (snd $3)) }
  | Expr3 { (fst $1, (snd $1)) }

Expr1 :: { (Fox.Abs.BNFC'Position, Fox.Abs.Expr) }
Expr1
  : Expr2 '&&' Expr1 { (fst $1, Fox.Abs.EAnd (fst $1) (snd $1) (snd $3)) }
  | Expr2 { (fst $1, (snd $1)) }

Expr :: { (Fox.Abs.BNFC'Position, Fox.Abs.Expr) }
Expr
  : Expr1 '||' Expr { (fst $1, Fox.Abs.EOr (fst $1) (snd $1) (snd $3)) }
  | Expr1 { (fst $1, (snd $1)) }

ListExpr :: { (Fox.Abs.BNFC'Position, [Fox.Abs.Expr]) }
ListExpr
  : {- empty -} { (Fox.Abs.BNFC'NoPosition, []) }
  | Expr { (fst $1, (:[]) (snd $1)) }
  | Expr ',' ListExpr { (fst $1, (:) (snd $1) (snd $3)) }

AddOp :: { (Fox.Abs.BNFC'Position, Fox.Abs.AddOp) }
AddOp
  : '+' { (uncurry Fox.Abs.BNFC'Position (tokenLineCol $1), Fox.Abs.Plus (uncurry Fox.Abs.BNFC'Position (tokenLineCol $1))) }
  | '-' { (uncurry Fox.Abs.BNFC'Position (tokenLineCol $1), Fox.Abs.Minus (uncurry Fox.Abs.BNFC'Position (tokenLineCol $1))) }

MulOp :: { (Fox.Abs.BNFC'Position, Fox.Abs.MulOp) }
MulOp
  : '*' { (uncurry Fox.Abs.BNFC'Position (tokenLineCol $1), Fox.Abs.Times (uncurry Fox.Abs.BNFC'Position (tokenLineCol $1))) }
  | '/' { (uncurry Fox.Abs.BNFC'Position (tokenLineCol $1), Fox.Abs.Div (uncurry Fox.Abs.BNFC'Position (tokenLineCol $1))) }
  | '%' { (uncurry Fox.Abs.BNFC'Position (tokenLineCol $1), Fox.Abs.Mod (uncurry Fox.Abs.BNFC'Position (tokenLineCol $1))) }

RelOp :: { (Fox.Abs.BNFC'Position, Fox.Abs.RelOp) }
RelOp
  : '<' { (uncurry Fox.Abs.BNFC'Position (tokenLineCol $1), Fox.Abs.LTH (uncurry Fox.Abs.BNFC'Position (tokenLineCol $1))) }
  | '<=' { (uncurry Fox.Abs.BNFC'Position (tokenLineCol $1), Fox.Abs.LE (uncurry Fox.Abs.BNFC'Position (tokenLineCol $1))) }
  | '>' { (uncurry Fox.Abs.BNFC'Position (tokenLineCol $1), Fox.Abs.GTH (uncurry Fox.Abs.BNFC'Position (tokenLineCol $1))) }
  | '>=' { (uncurry Fox.Abs.BNFC'Position (tokenLineCol $1), Fox.Abs.GE (uncurry Fox.Abs.BNFC'Position (tokenLineCol $1))) }
  | '==' { (uncurry Fox.Abs.BNFC'Position (tokenLineCol $1), Fox.Abs.EQU (uncurry Fox.Abs.BNFC'Position (tokenLineCol $1))) }
  | '!=' { (uncurry Fox.Abs.BNFC'Position (tokenLineCol $1), Fox.Abs.NE (uncurry Fox.Abs.BNFC'Position (tokenLineCol $1))) }

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

-- Entrypoints

pProgram :: [Token] -> Err Fox.Abs.Program
pProgram = fmap snd . pProgram_internal
}
