open Ast
open Format
open Lexing

let labeled_inst ?(label = "") inst =
  match label with
  | "" -> [inst]
  | _ -> ["LABEL " ^ label ^ ";" ^ inst]

let rec pos_list l elt =
  match l with
    | [] -> 0
    | x :: xs -> if x = elt then 0 else (1 + pos_list xs elt)

let rec reverse_list l =
  match l with
    | [] -> []
    | x :: xs -> reverse_list xs @ [x]

let localisation pos file =
  let l = pos.pos_lnum in
  let c = pos.pos_cnum - pos.pos_bol + 1 in
    eprintf "File \"%s\", line %d, characters %d-%d:@." file l (c-1) c

let extract_in_file file pos1 pos2 =
  let current_line = ref "" in
  let l1 = pos1.pos_lnum in
  let l2 = pos2.pos_lnum in
  try
    let ic = open_in file in
      eprintf "---- view in file ----@.";
      for i=0 to (l2-1) do
        current_line := input_line ic;
        if i >= (l1-1) && i <= (l2-1) then eprintf "%s@." !current_line
      done;
      eprintf "----@.";
      close_in ic;
  with exc -> ()

let counter_from n =
  let r = ref (n-1) in fun () -> incr r; !r

let ast_printer ast_stmt =
  let pos_printer pos =
    (* pos.pos_fname - pos.pos_bol *)
    eprintf "lnum:%d cnum:%d " pos.pos_lnum pos.pos_cnum
  in
  let loc_printer (lp_start,lp_end) =
    eprintf "( Startpos:"; pos_printer lp_start;
    eprintf "Endpos:"; pos_printer lp_end; eprintf ") "
  in
  let type_printer t =
    match t with
    | Tunit -> eprintf "Type:unit "
    | Tbool -> eprintf "Type:bool "
    | Tint -> eprintf "Type:int "
    | Tabool -> eprintf "Type:bool_array "
    | Taint -> eprintf "Type:int_array "
    | Tunknown -> eprintf "Type:unknown "
  in
  let ident_printer (typ,i) =
    eprintf "ident:%s " i;
    type_printer typ
  in
  let cst_printer c =
    match c with
    | Cunit -> eprintf "Cst:unit "
    | Cbool b -> eprintf "Cst:bool %b " b
    | Cint i -> eprintf "Cst:int %d " i
  in
  let unop_printer u =
    match u with
    | Unot -> eprintf "Unop:not "
  in
  let binop_printer b =
    match b with
    | Badd -> eprintf "Binop:add "
    | Bsub -> eprintf "Binop:sub "
    | Bmul -> eprintf "Binop:mul "
    | Bdiv -> eprintf "Binop:div "
    | Beq -> eprintf "Binop:eq "
    | Bneq -> eprintf "Binop:neq "
    | Blt -> eprintf "Binop:lt "
    | Ble -> eprintf "Binop:le "
    | Bgt -> eprintf "Binop:gt "
    | Bge -> eprintf "Binop:ge "
    | Band -> eprintf "Binop:and "
    | Bor -> eprintf "Binop:or "
  in
  let rec expr_printer e =
    eprintf "@[";
    begin match e with
    | Ecst (loc,typ,c) -> eprintf "Expr:cst > "; loc_printer loc; type_printer typ; cst_printer c
    | Eident (loc,typ,i) -> eprintf "Expr:ident > "; loc_printer loc; type_printer typ; ident_printer i
    | Eref (loc,typ,e) -> eprintf "Expr:ref > "; loc_printer loc; type_printer typ; expr_printer e
    | Ederef (loc,typ,i) -> eprintf "Expr:deref > "; loc_printer loc; type_printer typ; ident_printer i
    | Eunop (loc,typ,u,e) -> eprintf "Expr:unop > "; loc_printer loc; type_printer typ; unop_printer u; expr_printer e
    | Ebinop (loc,typ,bi,e1,e2) ->
        eprintf "Expr:binop > "; loc_printer loc; type_printer typ; binop_printer bi; expr_printer e1; expr_printer e2
    | Earray (loc,typ,l) ->
        eprintf "Expr:array > "; loc_printer loc; type_printer typ; expr_list_printer l
    | Eaget (loc,typ,i,e) ->
        eprintf "Expr:array_get > "; loc_printer loc; type_printer typ; ident_printer i; expr_printer e
    | Easize (loc,typ,i) -> eprintf "Expr:array_size > "; loc_printer loc; type_printer typ; ident_printer i
    | Erand (loc,typ,e1,e2) ->
        eprintf "Expr:rand > "; loc_printer loc; type_printer typ; expr_printer e1; expr_printer e2
    end;
    eprintf "@]"
  and expr_list_printer l =
    List.iter expr_printer l
  and stmt_printer s =
    eprintf "@[";
    begin match s with
    | Sassign (loc,i,e,s) ->
        eprintf "Stmt:asign > "; loc_printer loc; ident_printer i; expr_printer e; stmt_printer s
    | Srefassign (loc,i,e) -> eprintf "Stmt:refasign > "; loc_printer loc; ident_printer i; expr_printer e
    | Saassign (loc,i,e1,e2) -> 
        eprintf "Stmt:aasign > "; loc_printer loc; ident_printer i; expr_printer e1; expr_printer e1
    | Sblock b -> eprintf "Stmt:block > "; block_printer b
    | Sif (loc,e,s1,s2) -> 
        eprintf "Stmt:if > "; loc_printer loc; expr_printer e; stmt_printer s1; stmt_printer s2
    | Swhile (loc, e, b) -> eprintf "Stmt:while > "; expr_printer e; block_printer b
    | Sfor (loc,s1,e,s2,b) ->
        eprintf "Stmt:for > "; loc_printer loc; stmt_printer s1; expr_printer e; stmt_printer s2; block_printer b
    | Sprint e -> eprintf "Stmt:print > "; expr_printer e
    | Sprintall loc -> eprintf "Stmt:printall"; loc_printer loc
    | Sexit -> eprintf "Stmt:exit"
    | Sskip -> eprintf "Stmt:skip"
    end;
    eprintf "@]"
  and block_printer b =
    eprintf "@[";
    match b with
    | Bstmt s -> stmt_printer s
    | Bseq_l (s,b) -> stmt_printer s; block_printer b
    | Bseq_r (b,s) -> block_printer b; stmt_printer s
    ;
    eprintf "@]"
  in
    stmt_printer ast_stmt;
    eprintf "@."
