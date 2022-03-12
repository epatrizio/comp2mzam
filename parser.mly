/* Syntactic analyzer */

%{
%}

(*
[] SQuare brackets LSQ RSQ
() Parenthesis LP RP
{} CUrly brackets LCU RCU
*)

%token <Ast.constant> CST
%token LET IN REF BEGIN END IF THEN ELSE WHILE DO DONE PRINT ARRAY_SIZE AND OR NOT
%token EOF
%token COMMA SEMICOLON EXCL LP RP LSQ RSQ LCU RCU
%token EQUAL REF_EQUAL CMP_EQ CMP_NEQ CMP_LT CMP_LE CMP_GT CMP_GE
%token PLUS MINUS MULT DIV
%token<string> IDENT

(* Not useful > for now always parentheses
%left PLUS MINUS
%left MULT DIV
*)

%start prog

%type <Ast.stmt> prog
%type <Ast.block> block
%type <Ast.stmt> stmt
%type <Ast.expr> expr
%type <Ast.expr list> expr_list

%%

prog : s=stmt EOF { s };

stmt :
     | BEGIN b=block END { Ast.Sblock b }
     | LET i=IDENT EQUAL e=expr IN s=stmt { Ast.Sassign(i, e, s) }
     | i=IDENT REF_EQUAL e=expr { Ast.Srefassign(i, e) }
     | IF e=expr THEN s1=stmt ELSE s2=stmt { Ast.Sif (e, s1, s2) }
     | WHILE e=expr DO b=block DONE { Ast.Swhile (e, b) }
     | PRINT e=expr { Ast.Sprint e }
     ;

block :
     | s=stmt { Ast.Bstmt s }
     | s=stmt SEMICOLON b=block { Ast.Bseq (s, b) }
     ;

expr :
     | c=CST { Ast.Ecst c }
     | i=IDENT { Ast.Eident i }
     | LP NOT e=expr RP { Ast.Eunop (Unot, e) }
     | LP e1=expr PLUS e2=expr RP { Ast.Ebinop (Badd, e1, e2) }
     | LP e1=expr MINUS e2=expr RP { Ast.Ebinop (Bsub, e1, e2) }
     | LP e1=expr MULT e2=expr RP { Ast.Ebinop (Bmul, e1, e2) }
     | LP e1=expr DIV e2=expr RP { Ast.Ebinop (Bdiv, e1, e2) }
     | LP e1=expr CMP_EQ e2=expr RP { Ast.Ebinop (Beq, e1, e2) }
     | LP e1=expr CMP_NEQ e2=expr RP { Ast.Ebinop (Bneq, e1, e2) }
     | LP e1=expr CMP_LT e2=expr RP { Ast.Ebinop (Blt, e1, e2) }
     | LP e1=expr CMP_LE e2=expr RP { Ast.Ebinop (Ble, e1, e2) }
     | LP e1=expr CMP_GT e2=expr RP { Ast.Ebinop (Bgt, e1, e2) }
     | LP e1=expr CMP_GE e2=expr RP { Ast.Ebinop (Bge, e1, e2) }
     | LP e1=expr AND e2=expr RP { Ast.Ebinop (Band, e1, e2) }
     | LP e1=expr OR e2=expr RP { Ast.Ebinop (Bor, e1, e2) }
     | LP REF e=expr RP { Ast.Eref e }
     | LP EXCL i=IDENT RP { Ast.Ederef i }
     | LCU l=expr_list RCU { Ast.Earray l }
     | i=IDENT LSQ e=expr RSQ { Ast.Eaget (i, e) }
     | LP ARRAY_SIZE i=IDENT RP { Ast.Easize i }
     ;

expr_list :
     | e=expr { [e] }
     | e=expr COMMA l=expr_list { e :: l }
     ;

%%
