open Ast
open Utils

exception Error of string
let error message = raise (Error message)

let rec compile_expr ?(label = "") e env k li =
  match e with
  | Ecst (Cbool b) -> labeled_inst ~label:label (if b then "CONST 1" else "CONST 0") @ li
  | Ecst (Cint i) -> labeled_inst ~label:label ("CONST " ^ string_of_int i) @ li
  | Eident i ->
    if not (List.mem i env) then error ("unbound local var: " ^ i);
    ["ACC " ^ string_of_int k] @ li
  | Eunop (Unot,e) -> (compile_expr e env k li) @ ["PRIM not"] @ li
  | Ebinop (Badd,e1,e2) -> (compile_expr e2 env k li) @ ["PUSH"] @ (compile_expr e1 env (k+1) li) @ ["PRIM +"] @ li
  | Ebinop (Bsub,e1,e2) -> (compile_expr e2 env k li) @ ["PUSH"] @ (compile_expr e1 env (k+1) li) @ ["PRIM -"] @ li
  | Ebinop (Bmul,e1,e2) -> (compile_expr e2 env k li) @ ["PUSH"] @ (compile_expr e1 env (k+1) li) @ ["PRIM *"] @ li
  | Ebinop (Bdiv,e1,Ecst (Cint 0)) -> error "division by zero"
  | Ebinop (Bdiv,e1,e2) -> (compile_expr e2 env k li) @ ["PUSH"] @ (compile_expr e1 env (k+1) li) @ ["PRIM /"] @ li
  | Ebinop (Beq,e1,e2) -> (compile_expr e2 env k li) @ ["PUSH"] @ (compile_expr e1 env (k+1) li) @ ["PRIM ="] @ li
  | Ebinop (Bneq,e1,e2) -> (compile_expr e2 env k li) @ ["PUSH"] @ (compile_expr e1 env (k+1) li) @ ["PRIM <>"] @ li
  | Ebinop (Blt,e1,e2) -> (compile_expr e2 env k li) @ ["PUSH"] @ (compile_expr e1 env (k+1) li) @ ["PRIM <"] @ li
  | Ebinop (Ble,e1,e2) -> (compile_expr e2 env k li) @ ["PUSH"] @ (compile_expr e1 env (k+1) li) @ ["PRIM <="] @ li
  | Ebinop (Bgt,e1,e2) -> (compile_expr e2 env k li) @ ["PUSH"] @ (compile_expr e1 env (k+1) li) @ ["PRIM >"] @ li
  | Ebinop (Bge,e1,e2) -> (compile_expr e2 env k li) @ ["PUSH"] @ (compile_expr e1 env (k+1) li) @ ["PRIM >="] @ li
  | Ebinop (Band,e1,e2) -> (compile_expr e2 env k li) @ ["PUSH"] @ (compile_expr e1 env (k+1) li) @ ["PRIM &"] @ li
  | Ebinop (Bor,e1,e2) -> (compile_expr e2 env k li) @ ["PUSH"] @ (compile_expr e1 env (k+1) li) @ ["PRIM or"] @ li

let rec compile_stmt ?(label = "") s env li =
  match s with
  | Sassign(i,e,s) -> (compile_expr e env 0 li) @ ["PUSH"] @ (compile_stmt s (i :: env) li) @ ["POP"]
  | Sblock b -> compile_block ~label:label b env li
  | Sif (e,s1,s2) ->
      (compile_expr e env 0 li) @ ["BRANCHIFNOT f"] @ (compile_stmt s1 env li) @ ["BRANCH t"] @ (compile_stmt ~label:"f" s2 env li) @ labeled_inst ~label:"t" "STOP"
  | Sprint e -> (compile_expr ~label:label e env 0 li) @ ["PRIM print"]

and compile_block ?(label = "") b env li =
  match b with
  | Bstmt s -> compile_stmt ~label:label s env li
  | Bseq (s,b) -> compile_stmt ~label:label s env li @ compile_block ~label:label b env li

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
  let insts = compile_stmt stmt [] [] @ labeled_inst ~label:"end" "STOP" in
    insts_processing insts;
    close_out oc