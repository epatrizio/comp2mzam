(* Lexical analyzer *)

{
  open Lexing
  open Parser

  exception Lexing_error of string
  let error message = raise (Lexing_error message)
}

let letter = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']

let unit = "()"
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
  | '['           { LSQ }
  | ']'           { RSQ }
  | '{'           { LCU }
  | '}'           { RCU }
  | ','           { COMMA }
  | ';'           { SEMICOLON }
  | '!'           { EXCL }
  | ":="          { REF_EQUAL }
  | "let"         { LET }
  | "in"          { IN }
  | "ref"         { REF }
  | "begin"       { BEGIN }
  | "end"         { END }
  | "if"          { IF }
  | "then"        { THEN }
  | "else"        { ELSE }
  | "while"       { WHILE }
  | "do"          { DO }
  | "done"        { DONE }
  | "for"         { FOR }
  | "and"         { AND }
  | "or"          { OR }
  | "not"         { NOT }
  | "print"       { PRINT }
  | "array_size"  { ARRAY_SIZE }
  | "(*"          { comment lexbuf }
  | boolean as b  { CST (Cbool (bool_of_string b)) }
  | integer as s  { CST (Cint (int_of_string s)) }
  | unit          { CST Cunit }
  | ident as s    { IDENT(s) }
  | spaces        { token lexbuf }
  | newline       { token lexbuf }
  | eof           { EOF }
  | _ as lxm      { error ("unexpected character: " ^ (Char.escaped lxm)) }

and comment = parse
  | "*)"          { token lexbuf }
  | newline       { comment lexbuf }
  | _             { comment lexbuf }
  | eof           { error "unterminated comment" }