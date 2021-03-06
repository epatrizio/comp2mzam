(* Compiler *)

open Ast
open Utils

exception Error of string
let error message = raise (Error message)

let counter = counter_from 0

let rec compile_expr ?(label = "") e env k li =
  let compile_binop_expr e1 e2 prim env k li =
    (compile_expr e2 env k li) @ ["PUSH"] @ (compile_expr e1 env (k+1) li) @ ["PRIM " ^ prim]
  in
  let rec compile_array_expr la env k li =
    match la with
    | [] -> error "empty array"
    | [e] -> compile_expr e env k li
    | e::es -> compile_expr e env k li @ ["PUSH"] @ compile_array_expr es env k li
  in
  match e with
  | Ecst (Cbool b) -> labeled_inst ~label:label (if b then "CONST 1" else "CONST 0") @ li
  | Ecst (Cint i) -> labeled_inst ~label:label ("CONST " ^ string_of_int i) @ li
  | Ecst Cunit -> labeled_inst ~label:label ("CONST 0") @ li
  | Eident i ->
    if not (List.mem i env) then error ("unbound local var: " ^ i);
    ["ACC " ^ string_of_int ((pos_list env i) + k)] @ li
  | Eunop (Unot,e) -> compile_expr e env k li @ ["PRIM not"] @ li
  | Ebinop (Badd,e1,e2) -> compile_binop_expr e1 e2 "+" env k li @ li
  | Ebinop (Bsub,e1,e2) -> compile_binop_expr e1 e2 "-" env k li @ li
  | Ebinop (Bmul,e1,e2) -> compile_binop_expr e1 e2 "*" env k li @ li
  | Ebinop (Bdiv,e1,Ecst (Cint 0)) -> error "division by zero"
  | Ebinop (Bdiv,e1,e2) -> compile_binop_expr e1 e2 "/" env k li @ li
  | Ebinop (Beq,e1,e2) -> compile_binop_expr e1 e2 "=" env k li @ li
  | Ebinop (Bneq,e1,e2) -> compile_binop_expr e1 e2 "<>" env k li @ li
  | Ebinop (Blt,e1,e2) -> compile_binop_expr e1 e2 "<" env k li @ li
  | Ebinop (Ble,e1,e2) -> compile_binop_expr e1 e2 "<=" env k li @ li
  | Ebinop (Bgt,e1,e2) -> compile_binop_expr e1 e2 ">" env k li @ li
  | Ebinop (Bge,e1,e2) -> compile_binop_expr e1 e2 ">=" env k li @ li
  | Ebinop (Band,e1,e2) -> compile_binop_expr e1 e2 "&" env k li @ li
  | Ebinop (Bor,e1,e2) -> compile_binop_expr e1 e2 "or" env k li @ li
  | Eref e -> compile_expr e env k li @ ["MAKEBLOCK 1"] @ li
  | Ederef i -> compile_expr (Eident i) env k li @ ["GETFIELD 0"] @ li
  | Earray [] -> error "empty array"
  | Earray l -> compile_array_expr (List.rev l) env k li @ ["MAKEBLOCK " ^ string_of_int (List.length l)] @ li
  | Eaget (i,e) ->
    let tmp = "_tmp_" ^ string_of_int (counter ()) in
      compile_stmt (Sassign (tmp, e, Sif (Ebinop (Bge, (Eident tmp), (Easize i)), Sexit, Sskip))) env li @ 
        compile_expr e env k li @ ["PUSH"] @ compile_expr (Eident i) env (k+1) li @  ["GETVECTITEM"] @ li
  | Easize i -> compile_expr (Eident i) env k li @ ["VECTLENGTH"] @ li

  and compile_stmt ?(label = "") s env li =
  match s with
  | Sassign(i,e,s) ->
    if List.mem i env then error ("local var already bound: " ^ i);
    compile_expr e env 0 li @ ["PUSH"] @ compile_stmt s (i :: env) li @ ["POP"]
  | Srefassign(i,e) -> compile_expr e env 0 li @ ["PUSH"] @ compile_expr (Eident i) env 1 li @ ["SETFIELD 0"] @ li
  | Saassign(i,e1,e2) ->
    compile_expr e2 env 0 li @ ["PUSH"] @ compile_expr e1 env 1 li @ ["PUSH"] @ compile_expr (Eident i) env 2 li @ ["SETVECTITEM"] @ li
  | Sblock b -> compile_block ~label:label b env li
  | Sif (e,s1,s2) ->
    let sct = string_of_int (counter ()) in
    compile_expr e env 0 li @ ["BRANCHIFNOT f" ^ sct] @ compile_stmt s1 env li @ ["BRANCH t" ^ sct] @ compile_stmt ~label:("f"^sct) s2 env li @ labeled_inst ~label:("t"^sct) ""
  | Swhile (e,b) ->
    let sct = string_of_int (counter ()) in
    compile_expr e env 0 li @ labeled_inst ~label:("wcond"^sct) ("BRANCHIFNOT wdone"^sct) @ compile_block b env li @ compile_expr e env 0 li @ ["BRANCH wcond" ^ sct] @ labeled_inst ~label:("wdone"^sct) ""
  | Sfor (s1,e,s2,b) -> compile_stmt s1 env li @ compile_stmt (Swhile (e, Bseq_r (b,s2))) env li
  | Sprint e -> (compile_expr ~label:label e env 0 li) @ ["PRIM print"]
  | Sexit -> labeled_inst ~label:label ("STOP") @ li
  | Sskip -> labeled_inst ~label:label ("CONST 0") @ li

and compile_block ?(label = "") b env li =
  match b with
  | Bstmt s -> compile_stmt ~label:label s env li
  | Bseq_l (s,b) -> compile_stmt ~label:label s env li @ compile_block ~label:label b env li
  | Bseq_r (b,s) -> compile_block ~label:label b env li @ compile_stmt ~label:label s env li

let compile_prog stmt = compile_stmt stmt [] [] @ ["STOP"]

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
  let insts = compile_prog stmt in
    insts_processing insts;
    close_out oc