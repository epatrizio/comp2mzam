type unop =
  | Unot (* not e *)

type binop =
  | Badd | Bsub | Bmul | Bdiv           (* + - * / *)
  | Beq | Bneq | Blt | Ble | Bgt | Bge  (* == != < <= > >= *)
  | Band | Bor                          (* && || *)

type constant =
  | Cbool of bool
  | Cint of int

type expr =
  | Ecst of constant
  | Eunop of unop * expr
  | Ebinop of binop * expr * expr

and stmt =
  | Sblock of block
  | Sif of expr * stmt * stmt
  | Sprint of expr

and block =
  | Bstmt of stmt
  | Bseq of stmt * block

type prog = stmt
