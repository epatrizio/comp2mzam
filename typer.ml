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
    | [e] -> let typ, _ = type_expr env e in (typ,true)
    | e1 :: rlist ->
        let typ, _ = type_expr env e1 in
        let acc = (typ,true) in
          List.fold_left (
            fun (ty,ok) e -> let typ, _ = type_expr env e in (ty,(ok && (ty == typ)))
          ) acc rlist
  in
  let list_typing =
    List.map (fun e -> let _, ed = type_expr env e in ed)
  in
  match e with
  | Ecst (loc,_,Cunit) -> Tunit, Ecst (loc,Tunit,Cunit)
  | Ecst (loc,_,(Cbool b)) -> Tbool, Ecst (loc,Tbool,(Cbool b))
  | Ecst (loc,_,(Cint i)) -> Tint, Ecst (loc,Tint,(Cint i))
  | Eident (loc,_,(_,i)) -> begin
      try
        let typ = Tmap.find i env in
          typ, Eident (loc,typ,(typ,i))
      with Not_found -> error loc ("unbound local var: " ^ i)
    end
  | Eunop (loc,_,Unot,(Ecst (l,t,(Cbool b)))) -> Tbool, Eunop (loc,Tbool,Unot,(Ecst (l,Tbool,(Cbool b))))
  | Eunop (loc,_,Unot,(Ecst _)) -> error loc "not boolean type (unop)"
  | Eunop (loc,_,Unot,e) -> let typ, ed = type_expr env e in
      begin match typ with
      | Tbool -> Tbool, Eunop (loc,Tbool,Unot,ed)
      | _ -> error loc "not boolean type (unop)"
      end
  | Ebinop (loc,_,Band,e1,e2) ->
      let ty1, ed1 = type_expr env e1 in
      let ty2, ed2 = type_expr env e2 in
        begin match ty1 with
        | Tbool ->
            begin match ty2 with
            | Tbool -> Tbool, Ebinop (loc,Tbool,Band,ed1,ed2)
            | _ -> error loc "not boolean type (and binop)"
            end
        | _ -> error loc "not boolean type (and binop)"
        end
  | Ebinop (loc,_,Bor,e1,e2) ->
      let ty1, ed1 = type_expr env e1 in
      let ty2, ed2 = type_expr env e2 in
        begin match ty1 with
        | Tbool ->
            begin match ty2 with
            | Tbool -> Tbool, Ebinop (loc,Tbool,Bor,ed1,ed2)
            | _ -> error loc "not boolean type (or binop)"
            end
        | _ -> error loc "not boolean type (or binop)"
        end
  | Ebinop (loc,_,Badd,e1,e2) ->
      let ty1, ed1 = type_expr env e1 in
      let ty2, ed2 = type_expr env e2 in
        begin match ty1 with
        | Tint ->
            begin match ty2 with
            | Tint -> Tint, Ebinop (loc,Tint,Badd,ed1,ed2)
            | _ -> error loc "not integer type (add binop)"
            end
        | _ -> error loc "not integer type (add binop)"
        end
  | Ebinop (loc,_,Bsub,e1,e2) ->
      let ty1, ed1 = type_expr env e1 in
      let ty2, ed2 = type_expr env e2 in
        begin match ty1 with
        | Tint ->
            begin match ty2 with
            | Tint -> Tint, Ebinop (loc,Tint,Bsub,ed1,ed2)
            | _ -> error loc "not integer type (sub binop)"
            end
        | _ -> error loc "not integer type (sub binop)"
        end
  | Ebinop (loc,_,Bmul,e1,e2) ->
      let ty1, ed1 = type_expr env e1 in
      let ty2, ed2 = type_expr env e2 in
        begin match ty1 with
        | Tint ->
            begin match ty2 with
            | Tint -> Tint, Ebinop (loc,Tint,Bmul,ed1,ed2)
            | _ -> error loc "not integer type (mul binop)"
            end
        | _ -> error loc "not integer type (mul binop)"
        end
  | Ebinop (loc,_,Bdiv,_,Ecst (_,_,Cint 0)) -> error loc "division by zero"
  | Ebinop (loc,_,Bdiv,e1,e2) ->
      let ty1, ed1 = type_expr env e1 in
      let ty2, ed2 = type_expr env e2 in
        begin match ty1 with
        | Tint ->
            begin match ty2 with
            | Tint -> Tint, Ebinop (loc,Tint,Bdiv,ed1,ed2)
            | _ -> error loc "not integer type (div binop)"
            end
        | _ -> error loc "not integer type (div binop)"
        end
  | Ebinop (loc,_,Beq,e1,e2) ->
      let ty1, ed1 = type_expr env e1 in
      let ty2, ed2 = type_expr env e2 in
        if ty1 == ty2 then Tbool, Ebinop (loc,Tbool,Beq,ed1,ed2)
        else error loc "not identic type (equals comparaison binop)"
  | Ebinop (loc,_,Bneq,e1,e2) ->
      let ty1, ed1 = type_expr env e1 in
      let ty2, ed2 = type_expr env e2 in
        if ty1 == ty2 then Tbool, Ebinop (loc,Tbool,Bneq,ed1,ed2)
        else error loc "not identic type (diff comparaison binop)"
  | Ebinop (loc,_,Blt,e1,e2) ->
      let ty1, ed1 = type_expr env e1 in
      let ty2, ed2 = type_expr env e2 in
        if ty1 == Tint && ty2 == Tint then Tbool, Ebinop (loc,Tbool,Blt,ed1,ed2)
        else error loc "not integer type (< comparaison binop)"
  | Ebinop (loc,_,Ble,e1,e2) ->
      let ty1, ed1 = type_expr env e1 in
      let ty2, ed2 = type_expr env e2 in
        if ty1 == Tint && ty2 == Tint then Tbool, Ebinop (loc,Tbool,Ble,ed1,ed2)
        else error loc "not integer type (<= comparaison binop)"
  | Ebinop (loc,_,Bgt,e1,e2) ->
      let ty1, ed1 = type_expr env e1 in
      let ty2, ed2 = type_expr env e2 in
        if ty1 == Tint && ty2 == Tint then Tbool, Ebinop (loc,Tbool,Bgt,ed1,ed2)
        else error loc "not integer type (> comparaison binop)"
  | Ebinop (loc,_,Bge,e1,e2) ->
      let ty1, ed1 = type_expr env e1 in
      let ty2, ed2 = type_expr env e2 in
        if ty1 == Tint && ty2 == Tint then Tbool, Ebinop (loc,Tbool,Bge,ed1,ed2)
        else error loc "not integer type (>= comparaison binop)"
  | Eref (loc,_,e) -> let typ, ed = type_expr env e in typ, Eref (loc,typ,ed)
  | Ederef (loc,_,(_,i)) -> begin
      try
        let typ = Tmap.find i env in
          typ, Ederef (loc,typ,(typ,i))
      with Not_found -> error loc ("unbound local var: " ^ i)
    end
  | Earray (loc,_,[]) -> error loc "empty array"
  | Earray (loc,_,l) -> let (ty,b) = elts_same_types l in
      begin match b with
      | true -> begin match ty with
        | Tint -> Taint, Earray (loc,Taint,list_typing l)
        | Tbool -> Tabool, Earray (loc,Tabool,list_typing l)
        | Tunit -> error loc "unit type not authorized (array create)"
        | _ -> error loc "array of array type not supported (array create)"
        end
      | false -> error loc "not identic type (array create)"
      end
  | Eaget (loc,_,(_,i),e) -> begin
      try
        let typ = Tmap.find i env in
        let ty, ed = type_expr env e in
          begin match ty with
          | Tint -> begin match typ with
            | Taint -> Tint, Eaget (loc,Tint,(Taint,i),ed)
            | Tabool -> Tbool, Eaget (loc,Tbool,(Tabool,i),ed)
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
          | Taint -> Tint, Easize (loc,Tint,(Taint,i))
          | Tabool -> Tint, Easize (loc,Tint,(Tabool,i))
          | _ -> error loc "not array type (array_size primitive)"
          end
      with Not_found -> error loc ("unbound local var: " ^ i)
    end

and type_stmt env s =
  begin match s with
  | Sassign(loc,(_,i),e,s) ->
      let typ, ed = type_expr env e in
      let env = Tmap.add i typ env in
        Sassign(loc,(typ,i),ed,type_stmt env s)
  | Srefassign(loc,(_,i),e) -> begin
      try
        let typ = Tmap.find i env in
        let tye, ed = type_expr env e in
          if typ == tye then Srefassign(loc,(typ,i),ed)
          else error loc "not identic type (ref assign)"
      with Not_found -> error loc ("unbound local var: " ^ i)
    end
  | Saassign(loc,(_,i),e1,e2) -> begin
      try
        let typ = Tmap.find i env in
        let ty1, ed1 = type_expr env e1 in
        let ty2, ed2 = type_expr env e2 in
          begin match ty1 with
            | Tint -> begin match ty2 with
              | Tint ->
                  if typ == Taint then Saassign(loc,(typ,i),ed1,ed2)
                  else error loc "not integer type (array element assign)"
              | Tbool ->
                  if typ == Tabool then Saassign(loc,(typ,i),ed1,ed2)
                  else error loc "not boolean type (array element assign)"
              | _ -> error loc "incoherent type (array element assign)"
              end
            | _ -> error loc "not integer type (array assign accessor)"
          end
      with Not_found -> error loc ("unbound local var: " ^ i)
    end
  | Sblock b -> Sblock (type_block env b)
  | Swhile (loc,e,b) ->
      let typ, ed = type_expr env e in
        begin match typ with
        | Tbool -> Swhile (loc,ed,type_block env b)
        | _ -> error loc "not boolean type (while statement condition)"
        end
  | Sfor (loc,s1,e,s2,b) ->
      let typ, ed = type_expr env e in
        begin match typ with
        | Tbool -> Sfor (loc,
            type_stmt env s1,
            ed,
            type_stmt env s2,
            type_block env b)
        | _ -> error loc "not boolean type (for statement condition)"
        end
  | Sprint e ->
      let typ, ed = type_expr env e in Sprint ed (* print bool (0/1) or unit (0) is ok *)
  | Sif (loc,e,s1,s2) -> begin
      let typ, ed = type_expr env e in
        begin match typ with
        | Tbool -> Sif (loc,
              ed,
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
