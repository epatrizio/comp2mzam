%{
%}

%token <Ast.constant> CST
%token IF THEN ELSE PRINT NOT
%token EOF
%token LP RP
%token PLUS MINUS MULT DIV

(* Not useful > for now always parentheses
%left PLUS MINUS
%left MULT DIV
*)

%start prog

%type <Ast.stmt> prog
%type <Ast.stmt> stmt
%type <Ast.expr> expr

%%

prog : s=stmt EOF { s };

stmt :
     | IF e=expr THEN s1=stmt ELSE s2=stmt { Ast.Sif (e, s1, s2) }
     | PRINT e=expr { Ast.Sprint e }
     ;

expr :
     | c=CST { Ast.Ecst c }
     | LP NOT e=expr RP { Ast.Eunop (Unot, e) }
     | LP e1=expr PLUS e2=expr RP { Ast.Ebinop (Badd, e1, e2) }
     | LP e1=expr MINUS e2=expr RP { Ast.Ebinop (Ast.Bsub, e1, e2) }
     | LP e1=expr MULT e2=expr RP { Ast.Ebinop (Ast.Bmul, e1, e2) }
     | LP e1=expr DIV e2=expr RP { Ast.Ebinop (Ast.Bdiv, e1, e2) }
     ;

%%
