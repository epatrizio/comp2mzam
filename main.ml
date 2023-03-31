open Format
open Utils

let no_typing = ref false
let abs_inter_concrete = ref false
let abs_inter_constant = ref false

let debug = ref false

let in_file_name = ref ""
let set_file s = in_file_name := s

let options = [
  "--no-typing", Arg.Set no_typing, " Compile without typing checks";
  "--abs-inter-concrete", Arg.Set abs_inter_concrete, " Abstract interpretation - concrete domain";
  "--abs-inter-constant", Arg.Set abs_inter_constant, " Abstract interpretation - constant domain";
  "--debug", Arg.Set debug, " Debug mode (ast printer)"
]

let usage = "usage: ./c2mz [options] tests/bc_(test).txt"

module ConcreteAnalysis =
  Abstract_interpreter.Interprete(Domain_concrete.Concrete)

module ConstantAnalysis =
  Abstract_interpreter.Interprete(Domain_constant.Constants)

let process source_code_file no_typing abs_inter_concrete abs_inter_constant debug =
    let ic = open_in source_code_file in
    let lexbuf = Lexing.from_channel ic in
  try
    let ast = Parser.prog Lexer.token lexbuf in
      close_in ic;
      if abs_inter_concrete then
        ConcreteAnalysis.analyse_prog (Typer.typing ast)
      else (
        if abs_inter_constant then
          ConstantAnalysis.analyse_prog (Typer.typing ast)
        else (
          let ast = if not no_typing then Typer.typing ast else ast in
            if debug then ast_printer ast;
            Compiler.compile ast source_code_file
        )
      )
  with
    | Lexer.Lexing_error c -> 
        localisation (Lexing.lexeme_start_p lexbuf) source_code_file;
        eprintf "Lexical error: %s@." c;
        exit 1
    | Parser.Error ->
        localisation (Lexing.lexeme_start_p lexbuf) source_code_file;
        eprintf "Syntax error@.";
        exit 1
    | Typer.Error ((l1,l2),s) ->
        localisation l1 source_code_file;
        localisation l2 source_code_file;
        eprintf "Typing error: %s@." s;
        extract_in_file source_code_file l1 l2;
        exit 1
    | Compiler.Error ((l1,l2),s) ->
        localisation l1 source_code_file;
        localisation l2 source_code_file;
        eprintf "Compilation error: %s@." s;
        extract_in_file source_code_file l1 l2;
        exit 1

let _ =
  Arg.parse options set_file usage;
  if !in_file_name="" then begin
    eprintf "init error: missing test file name to compile!@.";
    Arg.usage options usage;
    exit 1
  end;
  process !in_file_name !no_typing !abs_inter_concrete !abs_inter_constant !debug
