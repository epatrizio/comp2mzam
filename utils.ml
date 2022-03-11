open Format
open Lexing

let labeled_inst ?(label = "") inst =
  match label with
  | "" -> [inst]
  | _ -> ["LABEL " ^ label ^ ";" ^ inst]

let rec pos_list l elt =
  match l with
    | [] -> 0
    | x :: xs -> if x = elt then 0 else (1 + pos_list xs elt)

let rec reverse_list l =
  match l with
    | [] -> []
    | x :: xs -> reverse_list xs @ [x]

let localisation pos file =
  let l = pos.pos_lnum in
  let c = pos.pos_cnum - pos.pos_bol + 1 in
    eprintf "File \"%s\", line %d, characters %d-%d:\n" file l (c-1) c
