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

let rec copy env = function
  | Unit -> Unit
  | Bool(b) -> Bool(b)
  | Int(i) -> Int(i)
  | Float(f) -> Float(f)
  | Not(e) -> Not(copy env e)
  | Neg(e) -> Neg(copy env e)
  | Add(e1, e2) -> Add(copy env e1, copy env e2)
  | Sub(e1, e2) -> Sub(copy env e1, copy env e2)
  | FNeg(e) -> FNeg(copy env e)
  | FAdd(e1, e2) -> FAdd(copy env e1, copy env e2)
  | FSub(e1, e2) -> FSub(copy env e1, copy env e2)
  | FMul(e1, e2) -> FMul(copy env e1, copy env e2)
  | FDiv(e1, e2) -> FDiv(copy env e1, copy env e2)
  | Eq(e1, e2) -> Eq(copy env e1, copy env e2)
  | LE(e1, e2) -> LE(copy env e1, copy env e2)
  | If(e1, e2, e3) -> If(copy env e1, copy env e2, copy env e3)
  | Let((x, t), e1, e2) -> Let((x, Type.copy env t), copy env e1, copy env e2)
  | Var(x) -> Var(x)
  | LetRec({ name = (x, t); args = yts; body = e1 }, e2) ->
      LetRec({ name = (x, Type.copy env t);
               args = List.map (fun (y, t) -> (y, Type.copy env t)) yts;
               body = copy env e1 }, copy env e2)
  | App(e1, es) -> App(copy env e1, List.map (copy env) es)
  | Tuple(es) -> Tuple(List.map (copy env) es)
  | LetTuple(xts, e1, e2) ->
      LetTuple(List.map (fun (x, t) -> (x, Type.copy env t)) xts, copy env e1, copy env e2)
  | Array(e1, e2) -> Array(copy env e1, copy env e2)
  | Get(e1, e2) -> Get(copy env e1, copy env e2)
  | Put(e1, e2, e3) -> Put(copy env e1, copy env e2, copy env e3)
  | List(es) -> List(List.map (copy env) es)
  | LAdd(e1, e2) -> LAdd(copy env e1, copy env e2)
  | Match(e1, e2, (x, xt), (y, yt), e3) ->
      Match(copy env e1, copy env e2, (x, Type.copy env xt), (y, Type.copy env yt), copy env e3)
