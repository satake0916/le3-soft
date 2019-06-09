(* OCaml のパーザジェネレータ Menhir に入力する文法定義である.
Menhir は .mly という拡張子のファイルに記述された BNF 風の文法定義から，
構文解析プログ ラムを生成する.定義の書き方は 6 章で説明する. *)

%{
open Syntax
%}

%token LPAREN RPAREN SEMISEMI 
%token PLUS MULT LT
%token IF THEN ELSE TRUE FALSE
%token LET IN EQ
%token RARROW FUN
%token REC

%token <int> INTV
%token <Syntax.id> ID

%start toplevel
%type <Syntax.program> toplevel
%%

toplevel :
    e=Expr SEMISEMI { Exp e }
  | LET x=ID EQ e=Expr SEMISEMI { Decl (x,e) }
  | LET REC x=ID EQ FUN y=ID RARROW e=Expr SEMISEMI { RecDecl (x, y, e) }

Expr :
    e=IfExpr { e }
  | e=LetExpr { e }
  | e=LTExpr { e }
  | e=FunExpr { e }
  | e=LetRecExpr { e }

LTExpr : 
    l=PExpr LT r=PExpr { BinOp (Lt, l, r) }
  | e=PExpr { e }

PExpr :
    l=PExpr PLUS r=MExpr { BinOp (Plus, l, r) }
  | e=MExpr { e }

MExpr : 
    l=MExpr MULT e2=AppExpr { BinOp (Mult, l, e2) }
  | e=AppExpr { e }

AppExpr :
    e1=AppExpr e2=AExpr { AppExp (e1, e2) }
  | e=AExpr { e }

AExpr :
    i=INTV { ILit i }
  | TRUE   { BLit true }
  | FALSE  { BLit false }
  | i=ID   { Var i }
  | LPAREN e=Expr RPAREN { e }
    

IfExpr :
    IF c=Expr THEN t=Expr ELSE e=Expr { IfExp (c, t, e) }

LetExpr :
    LET x=ID EQ e1=Expr IN e2=Expr { LetExp (x, e1, e2) }

LetRecExpr :
    LET REC x=ID EQ FUN y=ID RARROW e1=Expr IN e2=Expr { LetRecExp(x, y, e1, e2) }

FunExpr :
    FUN x=ID RARROW e=Expr { FunExp (x, e) }