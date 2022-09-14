(* Typer *)

open Ast
open Utils

exception Error of loc * string
let error loc message = raise (Error (loc,message))

module Tmap = Map.Make(String)

type environment = typ Tmap.t

let type_expr_deco e typ =
  match e with
  | Ecst (loc,_,c) -> Ecst (loc,typ,c)
  | Eident (loc,_,(_,i)) -> Eident (loc,typ,(typ,i))
  | Ederef (loc,_,(_,i)) -> Ederef (loc,typ,(typ,i))
  | Earray (loc,_,el) -> Earray (loc,typ,el)
  | Eaget (loc,_,i,e) -> Eaget (loc,typ,i,e)
  | Easize (loc,_,i) -> Easize (loc,typ,i)
  | exp -> exp

let rec type_expr env e =
  let elts_same_types = function
    | [] -> (Tunit,true)
    | [e] -> (type_expr env e,true)
    | e1 :: rlist -> let acc = (type_expr env e1,true) in
        List.fold_left (fun (ty,ok) e -> (ty,(ok && (ty == type_expr env e)))) acc rlist
  in
  match e with
  | Ecst (_,_,Cunit) -> Tunit
  | Ecst (_,_,(Cbool _)) -> Tbool
  | Ecst (_,_,(Cint _)) -> Tint
  | Eident (loc,_,(_,i)) -> begin
      try Tmap.find i env with Not_found -> error loc ("unbound local var: " ^ i)
    end
  | Eunop (_,Unot,(Ecst (_,_,(Cbool _)))) -> Tbool
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
  | Ederef (loc,_,(typ,i)) -> typ
  | Earray (loc,_,[]) -> error loc "empty array"
  | Earray (loc,_,l) -> let (ty,b) = elts_same_types l in
      begin match b with
      | true -> begin match ty with
        | Tint -> Taint
        | Tbool -> Tabool
        | Tunit -> error loc "unit type not authorized (array create)"
        | _ -> error loc "array of array type not supported (array create)"
        end
      | false -> error loc "not identic type (array create)"
      end
  | Eaget (loc,_,(_,i),e) -> begin
      try
        let typ = Tmap.find i env in
          begin match type_expr env e with
          | Tint -> begin match typ with
            | Taint -> Tint
            | Tabool -> Tbool
            | _ -> error loc "incoherent array type (array accessor)"
            end
          | _ -> error loc "not integer type (array accessor)"
          end
      with Not_found -> error loc ("unbound local var: " ^ i)
    end
  | Easize (loc,_,(_,i)) -> begin
      try
        let typ = Tmap.find i env in
          begin match typ with
          | Taint -> Tint
          | Tabool -> Tint
          | _ -> error loc "not array type (array_size primitive)"
          end
      with Not_found -> error loc ("unbound local var: " ^ i)
    end

and type_stmt env s =
  begin match s with
  | Sassign(loc,(_,i),e,s) ->
      let typ = type_expr env e in
      let env = Tmap.add i typ env in
        Sassign(loc,(typ,i),type_expr_deco e typ,type_stmt env s)
  | Srefassign(loc,(_,i),e) -> begin
      try
        let typ = Tmap.find i env in
        let tye = type_expr env e in
          if typ == tye then Srefassign(loc,(typ,i),type_expr_deco e tye)
          else error loc "not identic type (ref assign)"
      with Not_found -> error loc ("unbound local var: " ^ i)
    end
  | Saassign(loc,(_,i),e1,e2) -> begin
      try
        let typ = Tmap.find i env in
        let ty1 = type_expr env e1 in
        let ty2 = type_expr env e2 in
          begin match ty1 with
            | Tint -> begin match ty2 with
              | Tint ->
                  if typ == Taint then Saassign(loc,(typ,i),type_expr_deco e1 ty1,type_expr_deco e2 ty2)
                  else error loc "not integer type (array element assign)"
              | Tbool ->
                  if typ == Tabool then Saassign(loc,(typ,i),type_expr_deco e1 ty1,type_expr_deco e2 ty2)
                  else error loc "not boolean type (array element assign)"
              | _ -> error loc "incoherent type (array element assign)"
              end
            | _ -> error loc "not integer type (array assign accessor)"
          end
      with Not_found -> error loc ("unbound local var: " ^ i)
    end
  | Sblock b -> Sblock (type_block env b)
  | Swhile (loc,e,b) ->
      let typ = type_expr env e in
        begin match typ with
        | Tbool -> Swhile (loc,type_expr_deco e typ,type_block env b)
        | _ -> error loc "not boolean type (while statement condition)"
        end
  | Sfor (loc,s1,e,s2,b) ->
      let typ = type_expr env e in
        begin match typ with
        | Tbool -> Sfor (loc,
            type_stmt env s1,
            type_expr_deco e typ,
            type_stmt env s2,
            type_block env b)
        | _ -> error loc "not boolean type (for statement condition)"
        end
  | Sprint e -> Sprint (type_expr_deco e (type_expr env e)) (* print bool (0/1) or unit (0) is ok *)
  | Sif (loc,e,s1,s2) -> begin
      let typ = type_expr env e in
        begin match typ with
        | Tbool -> Sif (loc,
              type_expr_deco e typ,
              type_stmt env s1,
              type_stmt env s2
            )
        | _ -> error loc "not boolean type (if condition statement)"
        end
      end
  | Sexit -> Sexit
  | Sskip -> Sskip
  end

and type_block env b =
  match b with
  | Bstmt s -> Bstmt (type_stmt env s)
  | Bseq_l (s,b) -> Bseq_l (type_stmt env s, type_block env b)
  | Bseq_r (b,s) -> Bseq_r (type_block env b, type_stmt env s)

let typing ast_stmt =
  let env = Tmap.empty in
    type_stmt env ast_stmt
