(* Abstract Syntax Tree *)

type loc = Lexing.position * Lexing.position

type ident = string

type typ =
  | Tunit
  | Tbool
  | Tint
  | Tabool
  | Taint
  | Tunknown

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
  | Ecst of loc * typ * constant
  | Eident of loc * ident
  | Eref of loc * expr
  | Ederef of loc * ident
  | Eunop of loc * unop * expr
  | Ebinop of loc * binop * expr * expr
  | Earray of loc * expr list
  | Eaget of loc * ident * expr
  | Easize of loc * ident

type stmt =
  | Sassign of loc * ident * expr * stmt
  | Srefassign of loc * ident * expr
  | Saassign of loc * ident * expr * expr
  | Sblock of block
  | Sif of loc * expr * stmt * stmt
  | Swhile of loc * expr * block
  | Sfor of loc * stmt * expr * stmt * block
  | Sprint of expr
  | Sexit
  | Sskip

and block =
  | Bstmt of stmt
  | Bseq_l of stmt * block
  | Bseq_r of block * stmt

type prog = stmt
