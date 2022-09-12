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

let extract_in_file file pos1 pos2 =
  let current_line = ref "" in
  let l1 = pos1.pos_lnum in
  let l2 = pos2.pos_lnum in
  try
    let ic = open_in file in
      eprintf "---- view in file ----\n";
      for i=0 to (l2-1) do
        current_line := input_line ic;
        if i >= (l1-1) && i <= (l2-1) then eprintf "%s\n" !current_line
      done;
      eprintf "----\n";
      close_in ic;
  with exc -> ()

let counter_from n =
  let r = ref (n-1) in fun () -> incr r; !r
