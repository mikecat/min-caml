type t = (* MinCamlの型を表現するデータ型 (caml2html: type_t) *)
  | Unit
  | Bool
  | Int
  | Float
  | Fun of t list * t * (t list * t) list (* arguments are uncurried *)
  | MFun of t list * t * (t list * t) list ref * (t list * t -> unit) ref (* M : mutable *)
  | Tuple of t list
  | Array of t
  | List of t
  | Var of t option ref
  | Wild of int (* placeholder for uninstantiated type variables *)

let gentyp () = Var(ref None) (* 新しい型変数を作る *)

let wild_counter = ref 0
let genwild () =
  let c = !wild_counter in
  wild_counter := c + 1;
	Wild(c)

let rec copy env = function
  | Unit -> Unit
  | Bool -> Bool
  | Int -> Int
  | Float -> Float
  | Fun(xs, y, us) ->
      let us' = List.map (fun (ts, t) -> (List.map (copy env) ts, copy env t)) us in
      Fun(List.map (copy env) xs, copy env y, us')
  | MFun(xs, y, us, f) ->
      MFun(List.map (copy env) xs, copy env y, us, f) (* don't copy and reuse usage information *)
  | Tuple(xs) -> Tuple(List.map (copy env) xs)
  | Array(t) -> Array(copy env t)
  | List(t) -> List(copy env t)
  | Var(c) when List.mem_assq c !env -> Var(List.assq c !env)
  | Var({ contents = None } as c) ->
      let c' = ref None in
      env := ((c, c') :: !env);
      Var(c')
  | Var({ contents = Some(t) } as c) ->
      let c' = ref (Some(copy env t)) in
      env := ((c, c') :: !env);
      Var(c')
