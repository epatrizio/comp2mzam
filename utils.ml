let labeled_inst ?(label = "") inst =
  match label with
  | "" -> [inst]
  | _ -> ["LABEL " ^ label ^ ";" ^ inst]

let rec reverse_list l =
  match l with
    | [] -> []
    | x :: xs -> reverse_list xs @ [x]
