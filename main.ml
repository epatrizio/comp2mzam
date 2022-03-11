open Format
open Utils

let process source_code_file =
    let ic = open_in source_code_file in
    let lexbuf = Lexing.from_channel ic in
  try
    let ast = Parser.prog Lexer.token lexbuf in
      close_in ic;
      Compiler.compile ast source_code_file
  with
    | Lexer.Lexing_error c -> 
        localisation (Lexing.lexeme_start_p lexbuf) source_code_file;
        eprintf "Lexical error: %s@." c;
        exit 1
    | Parser.Error ->
        localisation (Lexing.lexeme_start_p lexbuf) source_code_file;
        eprintf "Syntax error@.";
        exit 1
    | Compiler.Error s ->
        eprintf "Compilation error: %s@." s;
        exit 1

let _ =
  for i = 1 to Array.length Sys.argv - 1 do
    process Sys.argv.(i)
  done
