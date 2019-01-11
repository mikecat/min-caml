type t = (* MinCaml�η���ɽ������ǡ����� (caml2html: type_t) *)
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

let gentyp () = Var(ref None) (* ���������ѿ����� *)
