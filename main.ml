let process source_code_file =
  let ic = open_in source_code_file in
  let lexbuf = Lexing.from_channel ic in
  let ast = Parser.prog Lexer.token lexbuf in
    Compiler.compile ast source_code_file

let _ =
  for i = 1 to Array.length Sys.argv - 1 do
    process Sys.argv.(i)
  done
