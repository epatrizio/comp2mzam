(* Abstract interpretation - Interval domain *)

open Ast
open Domain_value

open Format

module Intervals = (struct

  type bound =
    | Int of int  (* Z *)
    | PINF        (* +∞ *)
    | MINF        (* −∞ *)

  type t =
    | Itv of bound * bound
    | BOT

  let top = Itv (MINF, PINF)

  let bottom = BOT

  let const c = Itv (Int c, Int c)

  let rand x y =
    if x <= y then Itv (Int x, Int y) 
    else BOT

  let is_bottom x =
    x = BOT

  let lift1 f x =
    match x with
    | Itv (a, b) -> f a b
    | BOT -> BOT

  let lift2 f x y =
    match x, y with
    | Itv (x1, x2), Itv (y1, y2) -> Itv (f x1 y1, f x2 y2)
    | BOT, _ | _, BOT -> BOT

  (* bounds operations *)

  let bound_neg b =
    match b with
    | MINF -> PINF
    | PINF -> MINF
    | Int i -> Int (-i)
  
  let bound_add b1 b2 =
    match b1, b2 with
    | MINF,PINF | PINF,MINF -> invalid_arg "bound_add" (* (+∞) + (−∞) *)
    | MINF,_ | _,MINF -> MINF
    | PINF,_ | _,PINF -> PINF
    | Int i, Int j -> Int (i + j)
    
  let bound_mul b1 b2 =
    match b1, b2 with
    | Int z, MINF | Int z, PINF | MINF, Int z | PINF, Int z when z = 0 -> Int 0
    | Int i, PINF | PINF, Int i when i > 0 -> PINF
    | Int i, MINF | MINF, Int i when i > 0 -> MINF
    | Int i, PINF | PINF, Int i when i < 0 -> MINF
    | Int i, MINF | MINF, Int i when i < 0 -> PINF
    | Int i, Int j -> Int (i * j)
    | _, _ -> invalid_arg "bound_mul"

  let bound_div b1 b2 =
    match b1, b2 with
    | Int _, PINF | Int _, MINF -> Int 0
    | PINF, PINF -> Int 1
    | MINF, MINF -> Int 1
    | PINF, Int i when i > 0 -> PINF
    | PINF, Int i when i < 0 -> MINF
    | MINF, Int i when i > 0 -> MINF
    | MINF, Int i when i < 0 -> PINF
    | Int i, Int j -> Int (i / j)
    | _, _ -> invalid_arg "bound_div"
  
  let bound_cmp b1 b2 =
    match b1, b2 with
    | MINF,MINF | PINF,PINF -> 0
    | MINF,_ | _,PINF -> -1
    | PINF,_ | _,MINF -> 1
    | Int i, Int j -> Int.compare i j

  let bound_min_list (l : bound list) : bound =
    List.fold_left (fun acc e -> if bound_cmp acc e = -1 then acc else e) (List.hd l) l

  let bound_max_list (l : bound list) : bound =
    List.fold_left (fun acc e -> if bound_cmp acc e = 1 then acc else e) (List.hd l) l
  
  let neg x =
    lift1 (fun a b -> Itv (bound_neg b, bound_neg a)) x

  let add x y =
    lift2 (fun a b -> bound_add a b) x y

  let sub x y =
    lift2 (fun a b -> bound_add a b) x (neg y)

  let mul x y =
    match x, y with
    | _, BOT | BOT, _ -> BOT
    | Itv (x1, x2), Itv (y1, y2) ->
        let lb = [bound_mul x1 y1; bound_mul x1 y2; bound_mul x2 y1; bound_mul x2 y2] in
        let b1 = bound_min_list lb in
        let b2 = bound_max_list lb in
          Itv (b1, b2)

  let div_aux x y =
    match x, y with
    | Itv (x1, x2), Itv (Int y1, y2) when y1 > 0 ->
        let b1 = bound_min_list [bound_div x1 (Int y1); bound_div x1 y2] in
        let b2 = bound_max_list [bound_div x2 (Int y1); bound_div x2 y2] in
          Itv (b1, b2)
    | Itv (x1, x2), Itv (y1, Int y2) when y2 < 0 ->
        let b1 = bound_min_list [bound_div x2 y1; bound_div x2 (Int y2)] in
        let b2 = bound_max_list [bound_div x1 y1; bound_div x1 (Int y2)] in
          Itv (b1, b2)
    | _, Itv (Int x, Int y) when x = 0 && y = 0 -> BOT
    | _, _ -> BOT

  let join x y =
    match x, y with
    | BOT, i | i, BOT -> i
    | Itv (x1, x2), Itv (y1, y2) -> Itv (bound_min_list [x1; y1], bound_max_list [x2; y2])

  let meet x y =
    match x, y with
    | BOT, _ | _, BOT -> BOT
    | Itv (x1, x2), Itv (y1, y2) -> Itv (bound_max_list [x1; y1], bound_min_list [x2; y2])

  let widen x y =
    match x, y with
    | BOT, i | i, BOT -> i
    | Itv (x1, x2), Itv (y1, y2) ->
        let a = if bound_cmp x1 y1 <= 0 then x1 else MINF in
        let b = if bound_cmp x2 y2 >= 0 then x2 else PINF in
          Itv (a, b)

  let subset x y =
    match x, y with
    | BOT, _ -> true
    | _, BOT -> false
    | Itv (x1, x2), Itv (y1, y2) -> bound_cmp x1 y1 >= 0 && bound_cmp x2 y2 <= 0

  let div x y =
    match y with
    | Itv (a, b) when bound_cmp a (Int 0)>=0 || bound_cmp b (Int 0)<=0 ->
        div_aux x y
    | _ ->
        let y_pos = meet y (Itv (Int 1, PINF)) in
        let y_min = meet y (Itv (MINF, Int (-1))) in
          join (div_aux x y_pos) (div_aux x y_min)
  
  let eq x y =
    let i = meet x y in i, i

  let geq x y =
    match x, y with
    | BOT, _ -> BOT, BOT
    | _, BOT -> x, y
    | Itv (_, x2), Itv (y1, _) when bound_cmp x2 y1 < 0 -> BOT, BOT
    | Itv (x1, x2), Itv (y1, y2) -> Itv (bound_max_list [x1; y1], x2), Itv (y1, bound_min_list [x2; y2])

  let gt x y =
    match x, y with
    | BOT, _ -> BOT, BOT
    | _, BOT -> x, y
    | Itv (_, x2), Itv (y1, _) when bound_cmp x2 y1 <= 0 -> BOT, BOT
    | Itv (x1, x2), Itv (y1, y2) ->
        let max = bound_max_list [x1; bound_add y1 (Int 1)] in
        let min = bound_min_list [bound_add x2 (bound_neg (Int 1)); y2] in
          Itv (max, x2), Itv (y1, min)

  let neq x y =
    (* x != y <=> x>y OR x<y *)
    let a, b = gt x y in
    let c, d = gt y x in
      join a d, join b c

  let leq x y =
    let y', x' = geq y x in x', y'

  let lt x y =
    let y', x' = gt y x in x', y'

  let unary a uop =
    match uop with
    | _ -> assert false

  let binary a b bop =
    match bop with
    | Badd -> add a b
    | Bsub -> sub a b
    | Bmul -> mul a b
    | Bdiv -> div a b
    | _ -> assert false

  let compare a b bop =
    match bop with
    | Beq -> eq a b
    | Bneq -> neq a b
    | Bge -> geq a b
    | Bgt -> gt a b
    | Ble -> leq a b
    | Blt -> lt a b
    | _ -> assert false

  let print x =
    match x with
    | Itv (Int a, Int b) -> eprintf "[%s;%s]" (Int.to_string a) (Int.to_string b)
    | Itv (MINF, Int b) -> eprintf "[-∞;%s]" (Int.to_string b)
    | Itv (Int a, PINF) -> eprintf "[%s;+∞]" (Int.to_string a)
    | Itv (MINF, PINF) -> eprintf "[-∞;+∞]"
    | Itv (PINF, _) -> eprintf "⊥"
    | Itv (_, MINF) -> eprintf "⊥"
    | BOT -> eprintf "⊥"
  
end: VALUE_DOMAIN)
