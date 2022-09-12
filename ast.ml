(* Abstract Syntax Tree *)

type loc = Lexing.position * Lexing.position

type ident = string

type typ =
  | Tunit
  | Tbool
  | Tint
  | Tabool
  | Taint

type unop =
  | Unot (* not e *)

type binop =
  | Badd | Bsub | Bmul | Bdiv           (* + - * / *)
  | Beq | Bneq | Blt | Ble | Bgt | Bge  (* == != < <= > >= *)
  | Band | Bor                          (* && || *)

type constant =
  | Cunit
  | Cbool of bool
  | Cint of int

type expr =
  | Ecst of constant
  | Eident of ident
  | Eref of expr
  | Ederef of ident
  | Eunop of loc * unop * expr
  | Ebinop of binop * expr * expr
  | Earray of expr list
  | Eaget of ident * expr
  | Easize of ident

type stmt =
  | Sassign of ident * expr * stmt
  | Srefassign of ident * expr
  | Saassign of ident * expr * expr
  | Sblock of block
  | Sif of expr * stmt * stmt
  | Swhile of expr * block
  | Sfor of stmt * expr * stmt * block
  | Sprint of expr
  | Sexit
  | Sskip

and block =
  | Bstmt of stmt
  | Bseq_l of stmt * block
  | Bseq_r of block * stmt

type prog = stmt
