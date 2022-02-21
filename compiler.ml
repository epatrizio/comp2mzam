open Ast

let insts = []

let rec compile_expr e =
  match e with
  | Ecst (Cint i) -> ("CONST " ^ string_of_int i) :: insts

let compile_stmt s =
  match s with
  | Sprint e -> compile_expr e ; "PRIM print" :: insts

let compile s =
  let c = open_out "tests/build/bc.txt" in
  let fmt = Format.formatter_of_out_channel c in
    (* compile_stmt s;
    print_int (List.length insts); *)
    Format.fprintf fmt "end:\n";
    Format.fprintf fmt "\tSTOP\n";
    close_out c