{
  open Parser
}

let letter = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']

let boolean = "true" | "false"
let integer = ['0'-'9']+
let newline = '\r' | '\n' | "\r\n"
let spaces = [' ' '\t']+
let ident = letter (letter | digit | '_')*

rule token = parse
  | '+'           { PLUS }
  | '-'           { MINUS }
  | '*'           { MULT }
  | '/'           { DIV }
  | "=="          { CMP_EQ }
  | "!="          { CMP_NEQ }
  | '<'           { CMP_LT }
  | "<="          { CMP_LE }
  | '>'           { CMP_GT }
  | ">="          { CMP_GE }
  | '='           { EQUAL }
  | '('           { LP }
  | ')'           { RP }
  | ';'           { SEMICOLON }
  | "let"         { LET }
  | "in"          { IN }
  | "begin"       { BEGIN }
  | "end"         { END }
  | "if"          { IF }
  | "then"        { THEN }
  | "else"        { ELSE }
  | "and"         { AND }
  | "or"          { OR }
  | "not"         { NOT }
  | "print"       { PRINT }
  | boolean as b  { CST (Cbool (bool_of_string b)) }
  | integer as s  { CST (Cint (int_of_string s)) }
  | ident as s    { IDENT(s) }
  | spaces        { token lexbuf }
  | newline       { token lexbuf }
  | eof           { EOF }
  | _ as lxm      { Printf.printf "Unexpected character: %c" lxm; exit 0 }
