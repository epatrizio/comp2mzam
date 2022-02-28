open Ast
open Utils

let compile_expr e li =
  match e with
  | Ecst (Cint i) -> ("CONST " ^ string_of_int i) :: li

let compile_stmt s li =
  match s with
  | Sprint e -> "PRIM print" :: (compile_expr e li)

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
    let li_r = reverse_list li in
      List.map (fun s -> inst_processing s) li_r;
      Format.fprintf fmt "@."
  in
  let insts = "LABEL end;STOP" :: compile_stmt stmt [] in
    insts_processing insts;
    close_out oc