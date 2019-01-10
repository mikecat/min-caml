(* customized version of Map *)

module Vm =
  Map.Make
    (struct
      type t = Id.t * int
      let compare = compare
    end)
include Vm

let add_list xys env = List.fold_left (fun env (x, y) -> add x y env) env xys
let add_list2 xs ys env = List.fold_left2 (fun env x y -> add x y env) env xs ys
let add_v x ys env =
  fst (List.fold_left
    (fun (env, no) y -> (add (x, no) y env, no + 1))
    (env, 0) ys)
let add_list_v xyss env = List.fold_left (fun env (x, ys) -> add_v x ys env) env xyss
