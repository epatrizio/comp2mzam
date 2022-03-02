type constant =
  | Cint of int

type expr =
  | Ecst of constant

and stmt =
  | Sprint of expr

type prog = stmt
