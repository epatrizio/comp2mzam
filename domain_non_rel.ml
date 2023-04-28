(* Abstract interpretation - Non relational domain *)

open Ast
open Domain
open Domain_value

open Format

module NonRelational(D : VALUE_DOMAIN) = (struct

  module Env = Map.Make(String)

  type env = D.t Env.t

  type t =
  | Val of env
  | BOTTOM

  exception Empty

  type etree =
    | E_binop of binop * etree * D.t * etree * D.t
    | E_var of string * D.t
    | E_cst of D.t

  let rec eval_expr (e : expr) (m : env) : etree * D.t =
    match e with
    | Ecst (_, Tint, Cint c) ->
        let tc = D.const c in E_cst tc, tc
    | Eident (_, Tint, (Tint, var)) ->
        let v = Env.find var m in E_var (var, v), v
    | Eref (_, Tint, e) -> eval_expr e m
    | Ederef (_, Tint, (Tint, var)) ->
        let v = Env.find var m in E_var (var, v), v
    | Ebinop (_, Tint, Badd, e1, e2) ->
        let et1, v1 = eval_expr e1 m and et2, v2 = eval_expr e2 m in
          E_binop (Badd, et1, v1, et2, v2), D.binary v1 v2 Badd
    | Ebinop (_, Tint, Bsub, e1, e2) ->
        let et1, v1 = eval_expr e1 m and et2, v2 = eval_expr e2 m in
          E_binop (Bsub, et1, v1, et2, v2), D.binary v1 v2 Bsub
    | Ebinop (_, Tint, Bmul, e1, e2) ->
        let et1, v1 = eval_expr e1 m and et2, v2 = eval_expr e2 m in
          E_binop (Bmul, et1, v1, et2, v2), D.binary v1 v2 Bmul
    | Ebinop (_, Tint, Bdiv, e1, e2) ->
        let et1, v1 = eval_expr e1 m and et2, v2 = eval_expr e2 m in
          E_binop (Bdiv, et1, v1, et2, v2), D.binary v1 v2 Bdiv
    | Erand (_, Tint, Ecst (_, Tint, Cint i1), Ecst (_, Tint, Cint i2)) ->
        let r = D.rand i1 i2 in E_cst r, r
    | _ -> E_cst D.bottom, D.bottom

  let apply_compare (e1 : expr) (bop : binop) (e2 : expr) (m : env) : env =
    let rec refine (m : env) (et : etree) (tc : D.t) : env =
      match et with
      | E_binop (b, et1, tc1, et2, tc2) -> refine (refine m et1 tc1) et2 tc2
      | E_var (var, t) ->
          let v = D.meet t tc in
          if D.is_bottom v then raise Empty;
          Env.add var v m
      | E_cst t ->
          let v = D.meet t tc in
          if D.is_bottom v then raise Empty;
          m
    in
    let et1, c1 = eval_expr e1 m and et2, c2 = eval_expr e2 m in
    let cc1, cc2 = D.compare c1 c2 bop in
    refine (refine m et1 cc1) et2 cc2

  let init () = Val Env.empty

  let bottom () = BOTTOM

  let add_var m v = 
    match m with
    | BOTTOM -> BOTTOM
    | Val m -> Val (Env.add v (D.const 0) m)

  let del_var m v =
    match m with
    | BOTTOM -> BOTTOM
    | Val m -> Val (Env.remove v m)

  let assign m v e =
    match m with
    | BOTTOM -> BOTTOM
    | Val m ->
        let _, c = eval_expr e m in
          if D.is_bottom c then BOTTOM
          else Val (Env.add v c m)

  let compare m e1 op e2 =
    match m with
    | BOTTOM -> BOTTOM
    | Val m ->
        try Val (apply_compare e1 op e2 m)
        with Empty -> BOTTOM

  let join m1 m2 =
    match m1, m2 with
    | BOTTOM, x | x, BOTTOM -> x
    | Val m, Val n -> 
        Val (Env.union (fun _ a b -> Some (D.join a b)) m n)

  let widen m1 m2 =
    match m1, m2 with
    | BOTTOM, x | x, BOTTOM -> x
    | Val m, Val n ->
        Val (Env.union (fun _ a b -> Some (D.widen a b)) m n)

  let meet m1 m2 =
    match m1, m2 with
    | BOTTOM, x | x, BOTTOM -> x (* BOTTOM, x ? *)
    | Val m, Val n -> 
      try Val (
        Env.merge
          (fun _k a b -> match a, b with
            | Some a, Some b ->
              let r = D.meet a b in
                (if D.is_bottom r then raise Empty;
                Some r)
            | _, _ -> None
          ) m n)
      with Empty -> BOTTOM

  let subset m1 m2 =
    match m1, m2 with
    | BOTTOM, _ -> true
    | _, BOTTOM -> false
    | Val m, Val n -> 
        Env.fold (
          fun _ tc_m acc_m -> 
            Env.fold (
              fun _ tc_n acc_n -> acc_n || D.subset tc_m tc_n
            ) n acc_m
        ) m false

  let is_bottom m =
    m = BOTTOM

  let print lnum m var =
    eprintf "line %d: " lnum;
    match m with
    | BOTTOM -> eprintf "⊥"
    | Val m -> 
        eprintf "[";
        let v = Env.find var m in D.print v;
        eprintf "]@."

  let print_all lnum m =
    eprintf "line %d: " lnum;
    match m with
    | BOTTOM -> eprintf "⊥"
    | Val m -> 
        eprintf "[";
        let first = ref true in
          Env.iter (
            fun key v ->
              if !first then first := false else eprintf ",";
              eprintf " %s in " key;
              D.print v
          ) m;
        eprintf "]@."

end : DOMAIN)
