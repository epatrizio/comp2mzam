/* Syntactic analyzer */

%{
%}

(*
[] SQuare brackets LSQ RSQ
() Parenthesis LP RP
{} CUrly brackets LCU RCU
*)

%token <Ast.constant> CST
%token LET IN REF BEGIN END IF THEN ELSE WHILE DO DONE FOR AND OR NOT
%token PRINT PRINT_ALL RAND ARRAY_SIZE EXIT SKIP
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
     | LET i=IDENT EQUAL e=expr IN s=stmt { Ast.Sassign(($startpos,$endpos), (Ast.Tunknown, i), e, s) }
     | i=IDENT REF_EQUAL e=expr { Ast.Srefassign(($startpos,$endpos), (Ast.Tunknown, i), e) }
     | i=IDENT LSQ e1=expr RSQ REF_EQUAL e2=expr { Ast.Saassign(($startpos,$endpos), (Ast.Tunknown, i), e1, e2) }
     | IF e=expr THEN s1=stmt ELSE s2=stmt { Ast.Sif (($startpos,$endpos), e, s1, s2) }
     | WHILE e=expr DO b=block DONE { Ast.Swhile (($startpos,$endpos), e, b) }
     | FOR s1=stmt SEMICOLON e=expr SEMICOLON s2=stmt DO b=block DONE { Ast.Sfor (($startpos,$endpos), s1, e, s2, b) }
     | PRINT e=expr { Ast.Sprint e }
     | PRINT_ALL { Ast.Sprintall ($startpos,$endpos) }
     | EXIT { Ast.Sexit }
     | SKIP { Ast.Sskip }
     ;

block :
     | s=stmt { Ast.Bstmt s }
     | s=stmt SEMICOLON b=block { Ast.Bseq_l (s, b) }
     | b=block SEMICOLON s=stmt { Ast.Bseq_r (b, s) }
     ;

expr :
     | c=CST { Ast.Ecst (($startpos,$endpos), Ast.Tunknown, c) }
     | i=IDENT { Ast.Eident (($startpos,$endpos), Ast.Tunknown, (Ast.Tunknown, i)) }
     | LP NOT e=expr RP { Ast.Eunop (($startpos,$endpos), Ast.Tunknown, Unot, e) }
     | LP e1=expr PLUS e2=expr RP { Ast.Ebinop (($startpos,$endpos), Ast.Tunknown, Badd, e1, e2) }
     | LP e1=expr MINUS e2=expr RP { Ast.Ebinop (($startpos,$endpos), Ast.Tunknown, Bsub, e1, e2) }
     | LP e1=expr MULT e2=expr RP { Ast.Ebinop (($startpos,$endpos), Ast.Tunknown, Bmul, e1, e2) }
     | LP e1=expr DIV e2=expr RP { Ast.Ebinop (($startpos,$endpos), Ast.Tunknown, Bdiv, e1, e2) }
     | LP e1=expr CMP_EQ e2=expr RP { Ast.Ebinop (($startpos,$endpos), Ast.Tunknown, Beq, e1, e2) }
     | LP e1=expr CMP_NEQ e2=expr RP { Ast.Ebinop (($startpos,$endpos), Ast.Tunknown, Bneq, e1, e2) }
     | LP e1=expr CMP_LT e2=expr RP { Ast.Ebinop (($startpos,$endpos), Ast.Tunknown, Blt, e1, e2) }
     | LP e1=expr CMP_LE e2=expr RP { Ast.Ebinop (($startpos,$endpos), Ast.Tunknown, Ble, e1, e2) }
     | LP e1=expr CMP_GT e2=expr RP { Ast.Ebinop (($startpos,$endpos), Ast.Tunknown, Bgt, e1, e2) }
     | LP e1=expr CMP_GE e2=expr RP { Ast.Ebinop (($startpos,$endpos), Ast.Tunknown, Bge, e1, e2) }
     | LP e1=expr AND e2=expr RP { Ast.Ebinop (($startpos,$endpos), Ast.Tunknown, Band, e1, e2) }
     | LP e1=expr OR e2=expr RP { Ast.Ebinop (($startpos,$endpos), Ast.Tunknown, Bor, e1, e2) }
     | LP REF e=expr RP { Ast.Eref (($startpos,$endpos), Ast.Tunknown, e) }
     | LP EXCL i=IDENT RP { Ast.Ederef (($startpos,$endpos), Ast.Tunknown, (Ast.Tunknown, i)) }
     | LCU l=expr_list RCU { Ast.Earray (($startpos,$endpos), Ast.Tunknown, l) }
     | i=IDENT LSQ e=expr RSQ { Ast.Eaget (($startpos,$endpos), Ast.Tunknown, (Ast.Tunknown, i), e) }
     | LP ARRAY_SIZE i=IDENT RP { Ast.Easize (($startpos,$endpos), Ast.Tunknown, (Ast.Tunknown, i)) }
     | LP RAND e1=expr e2=expr RP { Ast.Erand (($startpos,$endpos), Ast.Tunknown, e1, e2) }
     ;

expr_list :
     | { [] }
     | e=expr { [e] }
     | e=expr COMMA l=expr_list { e :: l }
     ;

%%
