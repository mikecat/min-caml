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
  | Let of (Id.t * Type.t) * t * t
  | Var of Id.t
  | LetRec of fundef * t
  | App of t * t list
  | Tuple of t list
  | LetTuple of (Id.t * Type.t) list * t * t
  | Array of t * t
  | Get of t * t
  | Put of t * t * t
  | List of t list
  | LAdd of t * t
  | Match of t * t * (Id.t * Type.t) * (Id.t * Type.t) * t
and fundef = { name : Id.t * Type.t; args : (Id.t * Type.t) list; body : t }

let rec copy = function
  | Unit -> Unit
  | Bool(b) -> Bool(b)
  | Int(i) -> Int(i)
  | Float(f) -> Float(f)
  | Not(e) -> Not(copy e)
  | Neg(e) -> Neg(copy e)
  | Add(e1, e2) -> Add(copy e1, copy e2)
  | Sub(e1, e2) -> Sub(copy e1, copy e2)
  | FNeg(e) -> FNeg(copy e)
  | FAdd(e1, e2) -> FAdd(copy e1, copy e2)
  | FSub(e1, e2) -> FSub(copy e1, copy e2)
  | FMul(e1, e2) -> FMul(copy e1, copy e2)
  | FDiv(e1, e2) -> FDiv(copy e1, copy e2)
  | Eq(e1, e2) -> Eq(copy e1, copy e2)
  | LE(e1, e2) -> LE(copy e1, copy e2)
  | If(e1, e2, e3) -> If(copy e1, copy e2, copy e3)
  | Let((x, t), e1, e2) -> Let((x, Type.copy t), copy e1, copy e2)
  | Var(x) -> Var(x)
  | LetRec({ name = (x, t); args = yts; body = e1 }, e2) ->
      LetRec({ name = (x, Type.copy t); args = List.map (fun (y, t) -> (y, Type.copy t)) yts; body = copy e1 }, copy e2)
  | App(e1, es) -> App(copy e1, List.map copy es)
  | Tuple(es) -> Tuple(List.map copy es)
  | LetTuple(xts, e1, e2) -> LetTuple(List.map (fun (x, t) -> (x, Type.copy t)) xts, copy e1, copy e2)
  | Array(e1, e2) -> Array(copy e1, copy e2)
  | Get(e1, e2) -> Get(copy e1, copy e2)
  | Put(e1, e2, e3) -> Put(copy e1, copy e2, copy e3)
  | List(es) -> List(List.map copy es)
  | LAdd(e1, e2) -> LAdd(copy e1, copy e2)
  | Match(e1, e2, (x, xt), (y, yt), e3) -> Match(copy e1, copy e2, (x, Type.copy xt), (y, Type.copy yt), copy e3)
