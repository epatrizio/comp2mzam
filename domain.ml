(* Abstract interpretation - Domains signature (abstract or concrete) *)

open Ast

module type DOMAIN =
  sig

    (* abstract elements type *)
    type t

    (* initial empty environment *)
    val init: unit -> t

    (* empty set *)
    val bottom: unit -> t

    (* add a variable in environment *)
    val add_var: t -> string -> t

    (* remove a variable in environment *)
    val del_var: t -> string -> t

    (* assign an integer expression to a variable *)
    val assign: t -> string -> expr -> t

    (* filter environments *)
    val compare: t -> expr -> binop -> expr -> t

    (* abstract join *)
    val join: t -> t -> t

    (* abstract intersection *)
    val meet: t -> t -> t

    (* abstract widening *)
    val widen: t -> t -> t

    (* abstract element is included in another one *)
    val subset: t -> t -> bool

    (* abstract element is the empty set *)
    val is_bottom: t -> bool

    val print: int -> t -> string -> unit
    val print_all: int -> t -> unit

  end
