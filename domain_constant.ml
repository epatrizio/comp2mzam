(* Abstract interpretation - Constant domain *)

open Ast
open Domain

open Format

module Constants = (struct

  type t_const =
  | Cst of int
  | BOT         (* empty *)
  | TOP         (* all integers *)

  module Env = Map.Make(String)

  type env = t_const Env.t

  type t =
  | Val of env
  | BOTTOM

  exception Empty

  let t_lift1 f (x : t_const) : t_const =
    match x with
    | BOT -> BOT
    | TOP -> TOP
    | Cst a -> Cst (f a)

  let t_lift2 f (x : t_const) (y : t_const) : t_const =
    match x, y with
    | BOT,_ | _,BOT -> BOT
    | TOP,_ | _,TOP -> TOP
    | Cst a, Cst b -> Cst (f a b)

  let t_join a b =
    match a, b with
    | BOT, x | x, BOT -> x
    | Cst x, Cst y when x = y -> a
    | _ -> TOP

  let t_meet a b =
    match a, b with
    | TOP, x | x, TOP -> x
    | Cst x, Cst y when x = y -> a
    | _ -> BOT

  let t_subset a b =
    match a, b with
    | BOT, _ | _, TOP -> true
    | Cst x, Cst y -> x = y
    | _ -> false

  let t_eq a b =
    match a, b with
    | BOT, _ | _, BOT -> BOT, BOT
    | Cst va, Cst vb when va = vb -> a, b
    | Cst _, Cst _ -> BOT, BOT
    | TOP, x | x, TOP -> x, x

  let t_neq a b =
    match a, b with
    | Cst va, Cst vb when va != vb -> a, b
    | Cst _, Cst _ -> BOT, BOT
    | _, _ -> a, b

  let t_geq a b =
    match a, b with
    | Cst va, Cst vb when va >= vb -> a, b
    | Cst _, Cst _ -> BOT, BOT
    | _, _ -> a, b

  let t_gt a b =
    match a, b with
    | Cst va, Cst vb when va > vb -> a, b
    | Cst _, Cst _ -> BOT, BOT
    | _, _ -> a, b

  let t_leq a b =
    let b', a' = t_geq b a in a', b'

  let t_lt a b =
    let b', a' = t_gt b a in a', b'

  let t_print a =
    match a with
    | BOT -> eprintf "⊥"
    | TOP -> eprintf "⊤"
    | Cst x -> eprintf "{%s}" (Int.to_string x)

  type etree =
    | E_binop of binop * etree * t_const * etree * t_const
    | E_var of string * t_const
    | E_cst of t_const

  let rec eval_expr (e : expr) (m : env) : etree * t_const =
    match e with
    | Ecst (_, Tint, Cint c) ->
        let tc = Cst c in E_cst tc, tc
    | Eident (_, Tint, (Tint, var)) ->
        let v = Env.find var m in E_var (var, v), v
    | Eref (_, Tint, e) -> eval_expr e m
    | Ederef (_, Tint, (Tint, var)) ->
        let v = Env.find var m in E_var (var, v), v
    | Ebinop (_, Tint, Badd, e1, e2) ->
        let et1, v1 = eval_expr e1 m and et2, v2 = eval_expr e2 m in
          E_binop (Badd, et1, v1, et2, v2), t_lift2 (fun x y -> x + y) v1 v2
    | Ebinop (_, Tint, Bsub, e1, e2) ->
        let et1, v1 = eval_expr e1 m and et2, v2 = eval_expr e2 m in
          E_binop (Bsub, et1, v1, et2, v2), t_lift2 (fun x y -> x - y) v1 v2
    | Ebinop (_, Tint, Bmul, e1, e2) ->
        let et1, v1 = eval_expr e1 m and et2, v2 = eval_expr e2 m in
          if v1 = Cst 0 || v2 = Cst 0 then E_cst (Cst 0), Cst 0
          else E_binop (Bmul, et1, v1, et2, v2), t_lift2 (fun x y -> x * y) v1 v2
    | Ebinop (_, Tint, Bdiv, e1, e2) ->
      let et1, v1 = eval_expr e1 m and et2, v2 = eval_expr e2 m in
          if v2 = Cst 0 then E_cst BOT, BOT
          else E_binop (Bdiv, et1, v1, et2, v2), t_lift2 (fun x y -> x / y) v1 v2
    | Erand (_, Tint, Ecst (_, Tint, Cint i1), Ecst (_, Tint, Cint i2)) ->
        if i1 = i2 then E_cst (Cst i1), Cst i1
        else if i1 < i2 then E_cst TOP, TOP
        else E_cst BOT, BOT
    | _ -> E_cst BOT, BOT

  let apply_compare (e1 : expr) (bop : binop) (e2 : expr) (m : env) : env =
    let f_comp =
      match bop with
      | Beq -> t_eq
      | Bneq -> t_neq
      | Bgt -> t_gt
      | Bge -> t_geq
      | Blt -> t_lt
      | Ble -> t_leq
      | _ -> assert false
    in
    let rec refine (m : env) (et : etree) (tc : t_const) : env =
      match et with
      | E_binop (b, et1, tc1, et2, tc2) -> refine (refine m et1 tc1) et2 tc2
      | E_var (var, t) ->
          let v = t_meet t tc in
          if v = BOT then raise Empty;
          Env.add var v m
      | E_cst t ->
          let v = t_meet t tc in
          if v = BOT then raise Empty;
          m
    in
    let et1, c1 = eval_expr e1 m and et2, c2 = eval_expr e2 m in
    let cc1, cc2 = f_comp c1 c2 in
    refine (refine m et1 cc1) et2 cc2

  let init () = Val Env.empty

  let bottom () = BOTTOM

  let add_var m v = 
    match m with
    | BOTTOM -> BOTTOM
    | Val m -> Val (Env.add v (Cst 0) m)

  let del_var m v =
    match m with
    | BOTTOM -> BOTTOM
    | Val m -> Val (Env.remove v m)

  let assign m v e =
    match m with
    | BOTTOM -> BOTTOM
    | Val m ->
        let _, c = eval_expr e m in
          if c = BOT then BOTTOM
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
        Val (Env.union (fun k a b -> Some (t_join a b)) m n)

  let widen = join

  let meet m1 m2 =
    match m1, m2 with
    | BOTTOM, x | x, BOTTOM -> x (* BOTTOM, x ? *)
    | Val m, Val n -> 
      try Val (
        Env.merge
          (fun _k a b -> match a, b with
            | Some a, Some b ->
              let r = t_meet a b in
                (if r = BOT then raise Empty;
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
              fun _ tc_n acc_n -> acc_n || t_subset tc_m tc_n
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
        let v = Env.find var m in t_print v;
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
              t_print v
          ) m;
        eprintf "]@."

end: DOMAIN)
