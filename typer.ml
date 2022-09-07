(* Typer *)

open Ast

exception Error of string
let error message = raise (Error message)

module Tmap = Map.Make(String)

type environment = typ Tmap.t

let rec type_expr env e =
  match e with
  | Ecst Cunit -> Tunit
  | Ecst (Cbool b) -> Tbool
  | Ecst (Cint i) -> Tint
  | Eident i -> begin
      try Tmap.find i env with Not_found -> error ("unbound local var: " ^ i)
    end
  | Eunop (Unot,(Ecst (Cbool b))) -> Tbool
  | Eunop (Unot,(Ecst _)) -> error "not boolean type (unop)"
  | Eunop (Unot,e) -> type_expr env e
  | Ebinop (Band,e1,e2) -> begin match type_expr env e1 with
      | Tbool ->
          begin match type_expr env e2 with
          | Tbool -> Tbool
          | _ -> error "not boolean type (and binop)"
          end
      | _ -> error "not boolean type (and binop)"
      end
  | Ebinop (Bor,e1,e2) -> begin match type_expr env e1 with
      | Tbool ->
          begin match type_expr env e2 with
          | Tbool -> Tbool
          | _ -> error "not boolean type (or binop)"
          end
      | _ -> error "not boolean type (or binop)"
      end
  | Ebinop (Badd,e1,e2) -> begin match type_expr env e1 with
      | Tint ->
          begin match type_expr env e2 with
          | Tint -> Tint
          | _ -> error "not integer type (add binop)"
          end
      | _ -> error "not integer type (add binop)"
      end
  | Ebinop (Bsub,e1,e2) -> begin match type_expr env e1 with
      | Tint ->
          begin match type_expr env e2 with
          | Tint -> Tint
          | _ -> error "not integer type (sub binop)"
          end
      | _ -> error "not integer type (sub binop)"
      end
  | Ebinop (Bmul,e1,e2) -> begin match type_expr env e1 with
      | Tint ->
          begin match type_expr env e2 with
          | Tint -> Tint
          | _ -> error "not integer type (mul binop)"
          end
      | _ -> error "not integer type (mul binop)"
      end
  | Ebinop (Bdiv,e1,e2) -> begin match type_expr env e1 with
      | Tint ->
          begin match type_expr env e2 with
          | Tint -> Tint
          | _ -> error "not integer type (div binop)"
          end
      | _ -> error "not integer type (div binop)"
      end
  | Ebinop (Beq,e1,e2) ->
      let ty1 = type_expr env e1 in
      let ty2 = type_expr env e2 in
        if ty1 == ty2 then Tbool else error "not identic type (equals comparaison binop)"
  | Ebinop (Bneq,e1,e2) ->
    let ty1 = type_expr env e1 in
    let ty2 = type_expr env e2 in
      if ty1 == ty2 then Tbool else error "not identic type (diff comparaison binop)"
  | Ebinop (Blt,e1,e2) ->
      let ty1 = type_expr env e1 in
      let ty2 = type_expr env e2 in
        if ty1 == Tint && ty2 == Tint then Tbool else error "not integer type (< comparaison binop)"
  | Ebinop (Ble,e1,e2) ->
    let ty1 = type_expr env e1 in
    let ty2 = type_expr env e2 in
      if ty1 == Tint && ty2 == Tint then Tbool else error "not integer type (<= comparaison binop)"
  | Ebinop (Bgt,e1,e2) ->
    let ty1 = type_expr env e1 in
    let ty2 = type_expr env e2 in
      if ty1 == Tint && ty2 == Tint then Tbool else error "not integer type (> comparaison binop)"
  | Ebinop (Bge,e1,e2) ->
    let ty1 = type_expr env e1 in
    let ty2 = type_expr env e2 in
      if ty1 == Tint && ty2 == Tint then Tbool else error "not integer type (>= comparaison binop)"
  | _ -> error "not implemented (call compiler with --no-typing option)"

and type_stmt env s =
  match s with
  | Sassign(i,e,s) -> let env = Tmap.add i (type_expr env e) env in type_stmt env s
  | Sblock b -> type_block env b
  | Sprint e -> type_expr env e (* print bool (0/1) or unit (0) is ok *)
  | Sif (e,s1,s2) -> begin
      begin match type_expr env e with
      | Tbool -> Tbool
      | _ -> error "not boolean type (if condition statement)"
      end;
      type_stmt env s1;
      type_stmt env s2
    end
  | _ -> error "not implemented (call compiler with --no-typing option)"

and type_block env b =
  match b with
  | Bstmt s -> type_stmt env s
  | Bseq_l (s,b) -> type_stmt env s; type_block env b
  | Bseq_r (b,s) -> type_block env b; type_stmt env s

let typing stmt =
  let env = Tmap.empty in
    type_stmt env stmt
