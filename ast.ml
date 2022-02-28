type constant =
| Cbool of bool
| Cint of int

type expr =
  | Ecst of constant

and stmt =
  | Sprint of expr

type prog = stmt
