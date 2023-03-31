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
  
  let rec eval_expr (e : expr) (m : env) : t_const =
    match e with
    | Ecst (_, Tint, Cint c) -> Cst c
    | Eident (_, Tint, (Tint, var)) -> Env.find var m
    | Eref (_, Tint, e) -> eval_expr e m
    | Ederef (_, Tint, (Tint, var)) -> Env.find var m
    | Ebinop (_, Tint, Badd, e1, e2) ->
        let v1 = eval_expr e1 m and v2 = eval_expr e2 m in
          t_lift2 (fun x y -> x + y) v1 v2
    | Ebinop (_, Tint, Bsub, e1, e2) ->
        let v1 = eval_expr e1 m and v2 = eval_expr e2 m in
          t_lift2 (fun x y -> x - y) v1 v2
    | Ebinop (_, Tint, Bmul, e1, e2) ->
        let v1 = eval_expr e1 m and v2 = eval_expr e2 m in
          if v1 = Cst 0 || v2 = Cst 0 then Cst 0
          else t_lift2 (fun x y -> x * y) v1 v2
    | Ebinop (_, Tint, Bdiv, e1, e2) ->
        let v1 = eval_expr e1 m and v2 = eval_expr e2 m in
          if v2 = Cst 0 then BOT
          else t_lift2 (fun x y -> x / y) v1 v2
    | Erand (_, Tint, Ecst (_, Tint, Cint i1), Ecst (_, Tint, Cint i2)) ->
        if i1 = i2 then Cst i1
        else if i1 < i2 then TOP
        else BOT
    | _ -> BOT

  let eval_compare (e1 : expr) (bop : binop) (e2 : expr) (m : env) : bool =
    let f =
      match bop with
      | Beq -> t_eq
      | Bneq -> t_neq
      | Bgt -> t_gt
      | Bge -> t_geq
      | Blt -> t_lt
      | Ble -> t_leq
      | _ -> assert false
    in
    let s1 = eval_expr e1 m and s2 = eval_expr e2 m in
    false
    (* f s1 s2 *)
    (* todo *)

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
        let c = eval_expr e m in
          if c = BOT then BOTTOM
          else Val (Env.add v c m)

  let compare m e1 op e2 = m (* todo *)
    (* Env.filter (fun _ env -> eval_compare e1 op e2 env) m *)

  let join m1 m2 =
    match m1, m2 with
    | BOTTOM, x | x, BOTTOM -> x
    | Val m, Val n -> Val (Env.union (fun k a b -> Some (t_join a b)) m n) (* todo *)

  let widen = join

  let meet m1 m2 =
    match m1, m2 with
    | BOTTOM, x | x, BOTTOM -> BOTTOM
    | Val m, Val n -> Val (Env.union (fun _ a b -> Some (t_meet a b)) m n) (* todo *)

  let subset m1 m2 =
    match m1, m2 with
    | BOTTOM, _ -> true
    | _, BOTTOM -> false
    | Val m, Val n -> false (* Env.union (fun _ a b -> Some (t_subset a b)) m n *) (* todo *)

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
