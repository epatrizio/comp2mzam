open Ast

exception Error of string
let error message = raise (Error message)

let rec compile_expr e li =
  match e with
  | Ecst (Cbool b) -> (if b then "CONST 1" else "CONST 0") :: li
  | Ecst (Cint i) -> ("CONST " ^ string_of_int i) :: li
  | Eunop (Unot,e) -> (compile_expr e li) @ ["PRIM not"] @ li
  | Ebinop (Badd,e1,e2) -> (compile_expr e2 li) @ ["PUSH"] @ (compile_expr e1 li) @ ["PRIM +"] @ li
  | Ebinop (Bsub,e1,e2) -> (compile_expr e2 li) @ ["PUSH"] @ (compile_expr e1 li) @ ["PRIM -"] @ li
  | Ebinop (Bmul,e1,e2) -> (compile_expr e2 li) @ ["PUSH"] @ (compile_expr e1 li) @ ["PRIM *"] @ li
  | Ebinop (Bdiv,e1,e2) -> (compile_expr e2 li) @ ["PUSH"] @ (compile_expr e1 li) @ ["PRIM /"] @ li

let compile_stmt s li =
  match s with
  | Sprint e -> (compile_expr e li) @ ["PRIM print"]

let compile stmt in_file_name =
  let oc = open_out ("tests/build/bc_" ^ (Filename.basename in_file_name)) in
  let fmt = Format.formatter_of_out_channel oc in
  let inst_processing si =
    if String.starts_with "LABEL" si then
      let sl = String.split_on_char ';' si in
      let label_inst = List.hd sl in
      let label = String.sub label_inst 6 ((String.length label_inst) - 6) in
      let inst = List.nth sl 1 in
      Format.fprintf fmt "%s:\n\t%s\n" label inst
    else
      Format.fprintf fmt "\t%s\n" si
  in
  let insts_processing li =
    List.map (fun s -> inst_processing s) li;
    Format.fprintf fmt "@."
  in
  let insts = compile_stmt stmt [] @ ["LABEL end;STOP"] in
    insts_processing insts;
    close_out oc