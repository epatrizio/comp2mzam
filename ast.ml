(* Abstract Syntax Tree *)

type loc = Lexing.position * Lexing.position

type typ =
  | Tunit
  | Tbool
  | Tint
  | Tabool
  | Taint
  | Tunknown

type ident = typ * string

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
  | Eident of loc * typ * ident
  | Eref of loc * typ * expr
  | Ederef of loc * typ * ident
  | Eunop of loc * typ * unop * expr
  | Ebinop of loc * typ * binop * expr * expr
  | Earray of loc * typ * expr list
  | Eaget of loc * typ * ident * expr
  | Easize of loc * typ * ident
  | Erand of loc * typ * expr * expr

type stmt =
  | Sassign of loc * ident * expr * stmt
  | Srefassign of loc * ident * expr
  | Saassign of loc * ident * expr * expr
  | Sblock of block
  | Sif of loc * expr * stmt * stmt
  | Swhile of loc * expr * block
  | Sfor of loc * stmt * expr * stmt * block
  | Sprint of expr
  | Sprintall of loc
  | Sexit
  | Sskip

and block =
  | Bstmt of stmt
  | Bseq_l of stmt * block
  | Bseq_r of block * stmt

type prog = stmt
