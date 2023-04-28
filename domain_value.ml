(* Abstract interpretation - Value domain interface *)

open Ast

module type VALUE_DOMAIN =
  sig

    (* abstract element type *)
    type t

    (* unrestricted set *)
    val top: t

    (* empty set *)
    val bottom: t
    val is_bottom: t -> bool

    (* constant *)
    val const: int -> t

    (* interval *)
    val rand: int -> int -> t

    (* operations *)
    val join: t -> t -> t
    val meet: t -> t -> t
    val subset: t -> t -> bool
    val widen: t -> t -> t

    val unary: t -> unop -> t
    val binary: t -> t -> binop -> t
    val compare: t -> t -> binop -> (t * t)

    val print: t -> unit
end
