let rec reverse_list l =
  match l with
    | [] -> []
    | x :: xs -> reverse_list xs @ [x]
;;