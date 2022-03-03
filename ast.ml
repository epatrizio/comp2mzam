type unop =
  | Unot (* not e *)

type binop =
  | Badd | Bsub | Bmul | Bdiv    (* + - * / *)

type constant =
  | Cbool of bool
  | Cint of int

type expr =
  | Ecst of constant
  | Eunop of unop * expr
  | Ebinop of binop * expr * expr

and stmt =
  | Sif of expr * stmt * stmt
  | Sprint of expr

type prog = stmt
