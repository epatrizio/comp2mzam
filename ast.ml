type binop =
  | Badd | Bsub | Bmul | Bdiv    (* + - * / *)

type constant =
  | Cint of int

type expr =
  | Ecst of constant
  | Ebinop of binop * expr * expr

and stmt =
  | Sprint of expr

type prog = stmt
