(* Compiler *)

open Ast
open Utils

exception Error of loc * string
let error loc message = raise (Error (loc,message))

let counter = counter_from 0

let rec compile_expr ?(label = "") e env k li =
  let compile_binop_expr e1 e2 prim env k li =
    (compile_expr e2 env k li) @ ["PUSH"] @ (compile_expr e1 env (k+1) li) @ ["PRIM " ^ prim]
  in
  let rec compile_array_expr la env k li loc =
    match la with
    | [] -> error loc "empty array"
    | [e] -> compile_expr e env k li
    | e::es -> compile_expr e env k li @ ["PUSH"] @ compile_array_expr es env k li loc
  in
  match e with
  | Ecst (_,_,(Cbool b)) -> labeled_inst ~label:label (if b then "CONST 1" else "CONST 0") @ li
  | Ecst (_,_,(Cint i)) -> labeled_inst ~label:label ("CONST " ^ string_of_int i) @ li
  | Ecst (_,_,Cunit) -> labeled_inst ~label:label ("CONST 0") @ li
  | Eident (loc,_,(_,i)) ->
    if not (List.mem i env) then error loc ("unbound local var: " ^ i);
    ["ACC " ^ string_of_int ((pos_list env i) + k)] @ li
  | Eunop (_,Unot,e) -> compile_expr e env k li @ ["PRIM not"] @ li
  | Ebinop (_,Badd,e1,e2) -> compile_binop_expr e1 e2 "+" env k li @ li
  | Ebinop (_,Bsub,e1,e2) -> compile_binop_expr e1 e2 "-" env k li @ li
  | Ebinop (_,Bmul,e1,e2) -> compile_binop_expr e1 e2 "*" env k li @ li
  | Ebinop (loc,Bdiv,e1,Ecst (_,_,Cint 0)) -> error loc "division by zero"
  | Ebinop (_,Bdiv,e1,e2) -> compile_binop_expr e1 e2 "/" env k li @ li
  | Ebinop (_,Beq,e1,e2) -> compile_binop_expr e1 e2 "=" env k li @ li
  | Ebinop (_,Bneq,e1,e2) -> compile_binop_expr e1 e2 "<>" env k li @ li
  | Ebinop (_,Blt,e1,e2) -> compile_binop_expr e1 e2 "<" env k li @ li
  | Ebinop (_,Ble,e1,e2) -> compile_binop_expr e1 e2 "<=" env k li @ li
  | Ebinop (_,Bgt,e1,e2) -> compile_binop_expr e1 e2 ">" env k li @ li
  | Ebinop (_,Bge,e1,e2) -> compile_binop_expr e1 e2 ">=" env k li @ li
  | Ebinop (_,Band,e1,e2) -> compile_binop_expr e1 e2 "&" env k li @ li
  | Ebinop (_,Bor,e1,e2) -> compile_binop_expr e1 e2 "or" env k li @ li
  | Eref (_,e) -> compile_expr e env k li @ ["MAKEBLOCK 1"] @ li
  | Ederef (loc,_,(typ,i)) -> compile_expr (Eident (loc,typ,(typ,i))) env k li @ ["GETFIELD 0"] @ li
  | Earray (loc,_,[]) -> error loc "empty array"
  | Earray (loc,_,l) -> compile_array_expr (List.rev l) env k li loc @ ["MAKEBLOCK " ^ string_of_int (List.length l)] @ li
  | Eaget (loc,_,(typ,i),e) ->
    let tmp = "_tmp_" ^ string_of_int (counter ()) in
      compile_stmt (Sassign (loc, (typ,tmp), e, Sif (loc, Ebinop (loc, Bge, (Eident (loc,typ,(typ,tmp))), (Easize (loc,Tint,(typ,i)))), Sexit, Sskip))) env li @ 
        compile_expr e env k li @ ["PUSH"] @ compile_expr (Eident (loc,typ,(typ,i))) env (k+1) li @  ["GETVECTITEM"] @ li
  | Easize (loc,_,(typ,i)) -> compile_expr (Eident (loc,typ,(typ,i))) env k li @ ["VECTLENGTH"] @ li

  and compile_stmt ?(label = "") s env li =
  match s with
  | Sassign(loc,(_,i),e,s) ->
    if List.mem i env then error loc ("local var already bound: " ^ i);
    compile_expr e env 0 li @ ["PUSH"] @ compile_stmt s (i :: env) li @ ["POP"]
  | Srefassign(loc,(typ,i),e) -> compile_expr e env 0 li @ ["PUSH"] @ compile_expr (Eident (loc,typ,(typ,i))) env 1 li @ ["SETFIELD 0"] @ li
  | Saassign(loc,(typ,i),e1,e2) ->
    compile_expr e2 env 0 li @ ["PUSH"] @ compile_expr e1 env 1 li @ ["PUSH"] @ compile_expr (Eident (loc,typ,(typ,i))) env 2 li @ ["SETVECTITEM"] @ li
  | Sblock b -> compile_block ~label:label b env li
  | Sif (_,e,s1,s2) ->
    let sct = string_of_int (counter ()) in
    compile_expr e env 0 li @ ["BRANCHIFNOT f" ^ sct] @ compile_stmt s1 env li @ ["BRANCH t" ^ sct] @ compile_stmt ~label:("f"^sct) s2 env li @ labeled_inst ~label:("t"^sct) ""
  | Swhile (_,e,b) ->
    let sct = string_of_int (counter ()) in
    compile_expr e env 0 li @ labeled_inst ~label:("wcond"^sct) ("BRANCHIFNOT wdone"^sct) @ compile_block b env li @ compile_expr e env 0 li @ ["BRANCH wcond" ^ sct] @ labeled_inst ~label:("wdone"^sct) ""
  | Sfor (loc,s1,e,s2,b) -> compile_stmt s1 env li @ compile_stmt (Swhile (loc,e, Bseq_r (b,s2))) env li
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
    if String.starts_with ~prefix:"LABEL" si then
      let sl = String.split_on_char ';' si in
      let label_inst = List.hd sl in
      let label = String.sub label_inst 6 ((String.length label_inst) - 6) in
      let inst = List.nth sl 1 in
      Format.fprintf fmt "%s:\n\t%s\n" label inst
    else
      Format.fprintf fmt "\t%s\n" si
  in
  let insts_processing li =
    let _ = List.map (fun s -> inst_processing s) li in
    Format.fprintf fmt "@."
  in
  let insts = compile_prog stmt in
    insts_processing insts;
    close_out oc