(* Typer *)

open Ast
open Utils

exception Error of loc * string
let error loc message = raise (Error (loc,message))

module Tmap = Map.Make(String)

type environment = typ Tmap.t

let rec type_expr env e =
  let elts_same_types = function
    | [] -> (Tunit,true)
    | [e] -> (type_expr env e,true)
    | e1 :: rlist -> let acc = (type_expr env e1,true) in
        List.fold_left (fun (ty,ok) e -> (ty,(ok && (ty == type_expr env e)))) acc rlist
  in
  match e with
  | Ecst (_,Cunit) -> Tunit
  | Ecst (_,(Cbool b)) -> Tbool
  | Ecst (_,(Cint i)) -> Tint
  | Eident (loc,i) -> begin
      try Tmap.find i env with Not_found -> error loc ("unbound local var: " ^ i)
    end
  | Eunop (_,Unot,(Ecst (_,(Cbool b)))) -> Tbool
  | Eunop (loc,Unot,(Ecst _)) -> error loc "not boolean type (unop)"
  | Eunop (_,Unot,e) -> type_expr env e
  | Ebinop (loc,Band,e1,e2) -> begin match type_expr env e1 with
      | Tbool ->
          begin match type_expr env e2 with
          | Tbool -> Tbool
          | _ -> error loc "not boolean type (and binop)"
          end
      | _ -> error loc "not boolean type (and binop)"
      end
  | Ebinop (loc,Bor,e1,e2) -> begin match type_expr env e1 with
      | Tbool ->
          begin match type_expr env e2 with
          | Tbool -> Tbool
          | _ -> error loc "not boolean type (or binop)"
          end
      | _ -> error loc "not boolean type (or binop)"
      end
  | Ebinop (loc,Badd,e1,e2) -> begin match type_expr env e1 with
      | Tint ->
          begin match type_expr env e2 with
          | Tint -> Tint
          | _ -> error loc "not integer type (add binop)"
          end
      | _ -> error loc "not integer type (add binop)"
      end
  | Ebinop (loc,Bsub,e1,e2) -> begin match type_expr env e1 with
      | Tint ->
          begin match type_expr env e2 with
          | Tint -> Tint
          | _ -> error loc "not integer type (sub binop)"
          end
      | _ -> error loc "not integer type (sub binop)"
      end
  | Ebinop (loc,Bmul,e1,e2) -> begin match type_expr env e1 with
      | Tint ->
          begin match type_expr env e2 with
          | Tint -> Tint
          | _ -> error loc "not integer type (mul binop)"
          end
      | _ -> error loc "not integer type (mul binop)"
      end
  | Ebinop (loc,Bdiv,e1,e2) -> begin match type_expr env e1 with
      | Tint ->
          begin match type_expr env e2 with
          | Tint -> Tint
          | _ -> error loc "not integer type (div binop)"
          end
      | _ -> error loc "not integer type (div binop)"
      end
  | Ebinop (loc,Beq,e1,e2) ->
      let ty1 = type_expr env e1 in
      let ty2 = type_expr env e2 in
        if ty1 == ty2 then Tbool else error loc "not identic type (equals comparaison binop)"
  | Ebinop (loc,Bneq,e1,e2) ->
    let ty1 = type_expr env e1 in
    let ty2 = type_expr env e2 in
      if ty1 == ty2 then Tbool else error loc "not identic type (diff comparaison binop)"
  | Ebinop (loc,Blt,e1,e2) ->
      let ty1 = type_expr env e1 in
      let ty2 = type_expr env e2 in
        if ty1 == Tint && ty2 == Tint then Tbool else error loc "not integer type (< comparaison binop)"
  | Ebinop (loc,Ble,e1,e2) ->
    let ty1 = type_expr env e1 in
    let ty2 = type_expr env e2 in
      if ty1 == Tint && ty2 == Tint then Tbool else error loc "not integer type (<= comparaison binop)"
  | Ebinop (loc,Bgt,e1,e2) ->
    let ty1 = type_expr env e1 in
    let ty2 = type_expr env e2 in
      if ty1 == Tint && ty2 == Tint then Tbool else error loc "not integer type (> comparaison binop)"
  | Ebinop (loc,Bge,e1,e2) ->
    let ty1 = type_expr env e1 in
    let ty2 = type_expr env e2 in
      if ty1 == Tint && ty2 == Tint then Tbool else error loc "not integer type (>= comparaison binop)"
  | Eref (_,e) -> type_expr env e
  | Ederef (loc,i) -> type_expr env (Eident (loc,i))
  | Earray (loc,[]) -> error loc "empty array"
  | Earray (loc,l) -> let (ty,b) = elts_same_types l in
      begin match b with
      | true -> begin match ty with
        | Tint -> Taint
        | Tbool -> Tabool
        | Tunit -> error loc "unit type not authorized (array create)"
        | _ -> error loc "array of array type not supported (array create)"
        end
      | false -> error loc "not identic type (array create)"
      end
  | Eaget (loc,i,e) ->
      let tya = type_expr env (Eident (loc,i)) in
        begin match type_expr env e with
        | Tint -> begin match tya with
          | Taint -> Tint
          | Tabool -> Tbool
          | _ -> error loc "incoherent array type (array accessor)"
          end
        | _ -> error loc "not integer type (array accessor)"
        end
  | Easize (loc,i) -> begin match type_expr env (Eident (loc,i)) with
      | Taint -> Tint
      | Tabool -> Tint
      | _ -> error loc "not array type (array_size primitive)"
      end

and type_stmt env s =
  begin match s with
  | Sassign(_,i,e,s) -> let env = Tmap.add i (type_expr env e) env in type_stmt env s
  | Srefassign(loc,i,e) -> 
    let ty1 = type_expr env (Eident (loc,i)) in
    let ty2 = type_expr env e in
      if ty1 == ty2 then () else error loc "not identic type (ref assign)"
  | Saassign(loc,i,e1,e2) ->
      let tya = type_expr env (Eident (loc,i)) in
        begin match type_expr env e1 with
          | Tint -> begin match type_expr env e2 with
            | Tint -> if tya == Taint then () else error loc "not integer type (array element assign)"
            | Tbool -> if tya == Tabool then () else error loc "not boolean type (array element assign)"
            | _ -> error loc "incoherent type (array element assign)"
            end
          | _ -> error loc "not integer type (array assign accessor)"
          end
  | Sblock b -> type_block env b
  | Swhile (loc,e,b) -> begin match type_expr env e with
      | Tbool -> type_block env b
      | _ -> error loc "not boolean type (while statement condition)"
      end
  | Sfor (loc,s1,e,s2,b) -> begin match type_expr env e with
      | Tbool -> type_stmt env s1; type_stmt env s2; type_block env b
      | _ -> error loc "not boolean type (for statement condition)"
      end
  | Sprint e -> let _ = type_expr env e in () (* print bool (0/1) or unit (0) is ok *)
  | Sif (loc,e,s1,s2) -> begin
      begin match type_expr env e with
      | Tbool -> ()
      | _ -> error loc "not boolean type (if condition statement)"
      end;
      type_stmt env s1;
      type_stmt env s2
    end
  | Sexit -> ()
  | Sskip -> ()
  end

and type_block env b =
  match b with
  | Bstmt s -> type_stmt env s
  | Bseq_l (s,b) -> type_stmt env s; type_block env b
  | Bseq_r (b,s) -> type_block env b; type_stmt env s

let typing stmt =
  let env = Tmap.empty in
    type_stmt env stmt
