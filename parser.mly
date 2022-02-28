%{
%}

%token <Ast.constant> CST
%token PRINT
%token EOF
%token NEWLINE

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
     ;

%%
