type t = (* MinCamlの型を表現するデータ型 (caml2html: type_t) *)
  | Unit
  | Bool
  | Int
  | Float
  | Fun of t list * t (* arguments are uncurried *)
  | Tuple of t list
  | Array of t
  | List of t
  | Var of t option ref
  | Multi

let gentyp () = Var(ref None) (* 新しい型変数を作る *)
