{
  open Parser
}

let boolean = "true" | "false"
let integer = ['0'-'9']+
let newline = '\r' | '\n' | "\r\n"
let spaces = [' ' '\t']+

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
  | '('           { LP }
  | ')'           { RP }
  | "if"          { IF }
  | "then"        { THEN }
  | "else"        { ELSE }
  | "and"         { AND }
  | "or"          { OR }
  | "not"         { NOT }
  | "print"       { PRINT }
  | boolean as b  { CST (Cbool (bool_of_string b)) }
  | integer as s  { CST (Cint (int_of_string s)) }
  | spaces        { token lexbuf }
  | newline       { token lexbuf }
  | eof           { EOF }
  | _ as lxm      { Printf.printf "Unexpected character: %c" lxm; exit 0 }
