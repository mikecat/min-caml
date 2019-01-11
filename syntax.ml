type t = (* MinCamlの構文を表現するデータ型 (caml2html: syntax_t) *)
  | Unit
  | Bool of bool
  | Int of int
  | Float of float
  | Not of t
  | Neg of t
  | Add of t * t
  | Sub of t * t
  | FNeg of t
  | FAdd of t * t
  | FSub of t * t
  | FMul of t * t
  | FDiv of t * t
  | Eq of t * t
  | LE of t * t
  | If of t * t * t
  | Let of (Id.t * Type.t list) * t list * t
  | Var of Id.t * int
  | LetRec of (Id.t * Type.t list) * fundef list * t
  | App of t * t list * Type.t
  | Tuple of t list
  | LetTuple of (Id.t * Type.t list) list * t list * t
  | Array of t * t
  | Get of t * t
  | Put of t * t * t
  | List of t list
  | LAdd of t * t
  | Match of t list * t * (Id.t * Type.t list) * (Id.t * Type.t list) * t
and fundef = { name : Id.t * Type.t; args : (Id.t * Type.t) list; body : t }
