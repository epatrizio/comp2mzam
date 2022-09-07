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
  | Eident i -> Tmap.find i env
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
  | _ -> error "not implemented (call compiler with --no-typing option)"

and type_stmt env s =
  match s with
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

let typing stmt =
  let env = Tmap.empty in
    type_stmt env stmt
