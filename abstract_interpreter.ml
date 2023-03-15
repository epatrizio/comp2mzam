(* Abstract interpretation - interpreter by induction *)

open Ast
open Domain

module type INTERPRETER =
sig
  val analyse_prog: prog -> unit
end

module Interprete(D : DOMAIN) =
(struct

  type t = D.t

  let filter (a : t) (e : expr) (r : bool) : t =
    let rec expr_handle a (*e,_*) e r =
      match e with
      | Eunop (_, Tbool, Unot, e) -> expr_handle a e (not r)
      | Ebinop (_, Tbool, Band, e1, e2) -> 
          (if r then D.meet else D.join) (expr_handle a e1 r) (expr_handle a e2 r)
      | Ebinop (_, Tbool, Bor, e1, e2) -> 
          (if r then D.join else D.meet) (expr_handle a e1 r) (expr_handle a e2 r)
      | Ebinop (_, _, Beq, e1, e2) -> D.compare a e1 (if r then Beq else Bneq) e2
      | Ebinop (_, _, Bneq, e1, e2) -> D.compare a e1 (if r then Bneq else Beq) e2
      | Ebinop (_, _, Blt, e1, e2) -> D.compare a e1 (if r then Blt else Bgt) e2
      | Ebinop (_, _, Bgt, e1, e2) -> D.compare a e1 (if r then Bgt else Blt) e2
      | Ebinop (_, _, Ble, e1, e2) -> D.compare a e1 (if r then Ble else Bge) e2
      | Ebinop (_, _, Bge, e1, e2) -> D.compare a e1 (if r then Bge else Ble) e2
      | Ecst (_, Tbool, Cbool b) -> if b = r then a else D.bottom ()
      | _ -> D.bottom ()
    in
    expr_handle a e r

  let rec eval_stmt (a : t) (s : stmt) : t =
    let r = match s with
    | Sassign (_, (Tint, v_name), e, s) -> eval_stmt (D.assign a v_name e) s
    | Srefassign (_, (Tint, v_name), e) -> D.assign a v_name e
    | Sprint (Eident (l, Tint, (Tint, s))) -> D.print a [s]; a
    | Sprint (Ederef (l, Tint, (Tint, s))) -> D.print a [s]; a
    | Sblock (Bstmt s) -> eval_stmt a s
    | Sblock (Bseq_l (s, b)) -> eval_stmt (eval_stmt a s) (Sblock b)
    | Sblock (Bseq_r (b, s)) -> eval_stmt (eval_stmt a (Sblock b)) s
    | Sif (_, e, s1, s2) -> 
        let t = eval_stmt (filter a e true) s1 in
        let f = eval_stmt (filter a e false) s2 in
          D.join t f
    | Swhile (_, e, b) ->
        let rec fix (f:t -> t) (x:t) : t =
          let fx = f x in
            if D.subset fx x then fx
            else fix f fx
        in
        let f x = D.join a (eval_stmt (filter x e true) (Sblock b)) in
        let inv = fix f a in
          filter inv e false
    | Sfor (l,s1,e,s2,b) ->
        let a1 = eval_stmt a s1 in
          eval_stmt a1 (Swhile (l, e, (Bseq_r (b, s2))))
    | _ -> D.bottom ()
    in
      r

  let analyse_prog (stmt : prog) : unit =
    let _ = eval_stmt (D.init()) stmt in ()

end : INTERPRETER)
