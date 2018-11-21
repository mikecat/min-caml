type t = (* MinCaml�η���ɽ������ǡ����� (caml2html: type_t) *)
  | Unit
  | Bool
  | Int
  | Float
  | Fun of t list * t (* arguments are uncurried *)
  | Multi of t * t list ref
  | Tuple of t list
  | Array of t
  | List of t
  | Var of t option ref

let gentyp () = Var(ref None) (* ���������ѿ����� *)

let rec copy = function
  | Unit -> Unit
  | Bool -> Bool
  | Int -> Int
  | Float -> Float
  | Fun(xs, y) -> Fun(List.map copy xs, copy y)
  | Multi(g, us) -> Multi(copy g, ref (List.map copy !us))
  | Tuple(xs) -> Tuple(List.map copy xs)
  | Array(t) -> Array(copy t)
  | List(t) -> List(copy t)
  | Var({ contents = None }) -> Var(ref None)
  | Var({ contents = Some(t) }) -> Var(ref (Some(copy t)))
