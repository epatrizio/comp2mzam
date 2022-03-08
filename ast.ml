type ident = string

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
  | Eident of ident
  | Eref of expr
  | Ederef of ident
  | Eunop of unop * expr
  | Ebinop of binop * expr * expr

and stmt =
  | Sassign of ident * expr * stmt
  | Sblock of block
  | Sif of expr * stmt * stmt
  | Sprint of expr

and block =
  | Bstmt of stmt
  | Bseq of stmt * block

type prog = stmt
