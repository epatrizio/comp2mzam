{
  open Parser
}

let integer = ['0'-'9']+
let newline = '\r' | '\n' | "\r\n"
let spaces = [' ' '\t']+

rule token = parse
  | "print"       { PRINT }
  | integer as s  { CST (Cint (int_of_string s)) }
  | spaces        { token lexbuf }
  | newline       { NEWLINE }
  | eof           { EOF }
  | _ as lxm      { Printf.printf "Unexpected character: %c" lxm; exit 0 }