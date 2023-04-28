(* Abstract interpretation - Constant domain *)

open Ast
open Domain_value

open Format

module Constants = (struct

  type t =
  | Cst of int
  | BOT         (* empty *)
  | TOP         (* all integers *)

  let top = TOP

  let bottom = BOT

  let const c = Cst c

  let rand x y =
    if x = y then Cst x
    else if x < y then TOP
    else BOT

  let is_bottom x =
    x = BOT

  let lift1 f x =
    match x with
    | BOT -> BOT
    | TOP -> TOP
    | Cst a -> Cst (f a)

  let lift2 f x y =
    match x, y with
    | BOT,_ | _,BOT -> BOT
    | TOP,_ | _,TOP -> TOP
    | Cst a, Cst b -> Cst (f a b)

  let add = lift2 Int.add

  let sub = lift2 Int.sub

  let mul a b =
    if a = Cst 0 || b = Cst 0 then Cst 0
    else lift2 Int.mul a b

  let div a b =
    if b = Cst 0 then BOT
    else lift2 Int.div a b

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

  let join a b =
    match a, b with
    | BOT, x | x, BOT -> x
    | Cst x, Cst y when x = y -> a
    | _ -> TOP

  let widen = join

  let meet a b =
    match a, b with
    | TOP, x | x, TOP -> x
    | Cst x, Cst y when x = y -> a
    | _ -> BOT

  let subset a b =
    match a, b with
    | BOT, _ | _, TOP -> true
    | Cst x, Cst y -> x = y
    | _ -> false

  let eq a b =
    match a, b with
    | BOT, _ | _, BOT -> BOT, BOT
    | Cst va, Cst vb when va = vb -> a, b
    | Cst _, Cst _ -> BOT, BOT
    | TOP, x | x, TOP -> x, x

  let neq a b =
    match a, b with
    | Cst va, Cst vb when va != vb -> a, b
    | Cst _, Cst _ -> BOT, BOT
    | _, _ -> a, b

  let geq a b =
    match a, b with
    | Cst va, Cst vb when va >= vb -> a, b
    | Cst _, Cst _ -> BOT, BOT
    | _, _ -> a, b

  let gt a b =
    match a, b with
    | Cst va, Cst vb when va > vb -> a, b
    | Cst _, Cst _ -> BOT, BOT
    | _, _ -> a, b

  let leq a b =
    let b', a' = geq b a in a', b'

  let lt a b =
    let b', a' = gt b a in a', b'

  let compare a b bop =
    match bop with
    | Beq -> eq a b
    | Bneq -> neq a b
    | Bge -> geq a b
    | Bgt -> gt a b
    | Ble -> leq a b
    | Blt -> lt a b
    | _ -> assert false

  let print a =
    match a with
    | BOT -> eprintf "⊥"
    | TOP -> eprintf "⊤"
    | Cst x -> eprintf "{%s}" (Int.to_string x)

end: VALUE_DOMAIN)
