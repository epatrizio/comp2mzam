%{
%}

%token <Ast.constant> CST
%token PRINT
%token EOF
%token LP RP NEWLINE
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
     | PRINT e=expr NEWLINE { Ast.Sprint e }
     ;

expr :
     | c=CST { Ast.Ecst c }
     | LP e1=expr PLUS e2=expr RP { Ast.Ebinop (Badd, e1, e2) }
     | LP e1=expr MINUS e2=expr RP { Ast.Ebinop (Ast.Bsub, e1, e2) }
     | LP e1=expr MULT e2=expr RP { Ast.Ebinop (Ast.Bmul, e1, e2) }
     | LP e1=expr DIV e2=expr RP { Ast.Ebinop (Ast.Bdiv, e1, e2) }
     ;

%%
