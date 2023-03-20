(* Abstract interpretation - Concrete execution domain *)

open Ast
open Domain

open Format

module Concrete = (struct

  (* variables (strings) in a map *)
  module Env = Map.Make(String)

  (* environment : we consider integer value *)
  type env = int Env.t

  (* environments set *)
  module EnvSet =
    Set.Make
      (struct
        type t = env
        let compare = Env.compare Int.compare
      end)

  (* an element is a set of possible environments *)
  type t = EnvSet.t

  (* a set of integer values *)
  module ValSet = Set.Make
    (struct
      type t = int
      let compare = Int.compare
    end)

  let int_map (f : int -> int) (s : ValSet.t) : ValSet.t =
    ValSet.fold (fun x acc -> ValSet.add (f x) acc) s ValSet.empty

  let int2_map (f: int -> int -> int) (s1 : ValSet.t) (s2 : ValSet.t) : ValSet.t =
    ValSet.fold
      (fun x1 acc ->
        ValSet.fold
          (fun x2 acc -> ValSet.add (f x1 x2) acc) s2 acc
      ) s1 ValSet.empty

  let rec eval_expr (e : expr) (m : env) : ValSet.t =
    match e with
    | Ecst (_, Tint, Cint c) -> ValSet.singleton c
    | Eident (_, Tint, (Tint, var)) -> ValSet.singleton (Env.find var m)
    | Eref (_, Tint, e) -> eval_expr e m
    | Ederef (_, Tint, (Tint, var)) -> ValSet.singleton (Env.find var m)
    | Ebinop (_, Tint, Badd, e1, e2) ->
        let v1 = eval_expr e1 m and v2 = eval_expr e2 m in
          int2_map (fun x y -> x + y) v1 v2
    | Ebinop (_, Tint, Bsub, e1, e2) ->
        let v1 = eval_expr e1 m and v2 = eval_expr e2 m in
          int2_map (fun x y -> x - y) v1 v2
    | Ebinop (_, Tint, Bmul, e1, e2) ->
        let v1 = eval_expr e1 m and v2 = eval_expr e2 m in
          int2_map (fun x y -> x * y) v1 v2
    | Ebinop (_, Tint, Bdiv, e1, e2) ->
        let v1 = eval_expr e1 m and v2 = eval_expr e2 m in
          let v2 = ValSet.remove 0 v2 in int2_map (fun x y -> x / y) v1 v2
    | Erand (_, Tint, Ecst (_, Tint, Cint i1), Ecst (_, Tint, Cint i2)) ->
        let rec rand_set v set =
          if v > i2 then set
          else rand_set (v+1) (ValSet.add v set)
        in
        rand_set i1 ValSet.empty
    | _ -> ValSet.empty

  let eval_compare (e1 : expr) (bop : binop) (e2 : expr) (m : env) : bool =
    let f =
      match bop with
      | Beq -> Int.equal
      | Bneq -> fun x y -> not (Int.equal x y)
      | Blt -> fun x y -> x < y
      | Ble -> fun x y -> x <= y
      | Bgt -> fun x y -> x > y
      | Bge -> fun x y -> x >= y
      | _ -> fun _ _ -> false   (* todo *)
    in
    let s1 = eval_expr e1 m and s2 = eval_expr e2 m in
    ValSet.fold
      (fun v1 acc ->
        ValSet.fold
          (fun v2 acc ->
            (acc || (f v1 v2))
          ) s2 acc
      ) s1 false

  let env_set_map f m =
    EnvSet.fold (fun x acc -> EnvSet.add (f x) acc) m EnvSet.empty

  let init () = EnvSet.singleton Env.empty

  let bottom () = EnvSet.empty

  let add_var m v = env_set_map (Env.add v Int.zero) m

  let del_var m v = env_set_map (Env.remove v) m

  let assign m var e =
    EnvSet.fold
      (fun env acc ->
        let s = eval_expr e env in
        ValSet.fold
          (fun v acc ->
            EnvSet.add (Env.add var v env) acc
          ) s acc
      ) m EnvSet.empty

  let compare m e1 op e2 =
    EnvSet.filter (fun env -> eval_compare e1 op e2 env) m

  let join m1 m2 = EnvSet.union m1 m2

  let widen = join

  let meet m1 m2 = EnvSet.inter m1 m2

  let subset m1 m2 = EnvSet.subset m1 m2

  let is_bottom m = EnvSet.is_empty m

  let print lnum m var =
    eprintf "line %d: " lnum;
    eprintf "{ ";
    EnvSet.iter
      (fun env ->
        eprintf "[";
        eprintf "%s=%s" var (Int.to_string (Env.find var env));
        eprintf "]; "
      ) m;
    eprintf "}@."

  let print_all lnum m =
    eprintf "line %d: " lnum;
    eprintf "{ ";
    EnvSet.iter
      (fun env ->
        eprintf "[";
        Env.iter
          (fun var v ->
            eprintf "%s=%s;" var (Int.to_string v)
          ) env;
        eprintf "]; "
      ) m;
      eprintf "}@."

end: DOMAIN)
