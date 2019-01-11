open Syntax

(* 「同じ変数」の関係は破壊される *)
let rec copy_type t = match t with
    Type.Unit -> Type.Unit
  | Type.Bool -> Type.Bool
  | Type.Int -> Type.Int
  | Type.Float -> Type.Float
  | Type.Fun(t1s, t2) -> Type.Fun(List.map copy_type t1s, copy_type t2)
  | Type.Tuple(ts) -> Type.Tuple(List.map copy_type ts)
  | Type.Array(t) -> Type.Array(copy_type t)
  | Type.List(t) -> Type.List(copy_type t)
  | Type.Var({ contents = None }) -> Type.Var(ref None)
  | Type.Var({ contents = Some(t) }) -> Type.Var(ref (Some(copy_type t)))

let rec copy_type_n n t =
  let rec ctni n ret =
    if n <= 1 then (copy_type t) :: ret
              else ctni (n - 1) ((copy_type t) :: ret)
  in ctni n []

let rec g env e = match e with
    Unit -> Unit
  | Bool(v) -> Bool(v)
  | Int(v) -> Int(v)
  | Float(v) -> Float(v)
  | Not(e) -> Not(g env e)
  | Neg(e) -> Neg(g env e)
  | Add(e1, e2) -> Add(g env e1, g env e2)
  | Sub(e1, e2) -> Sub(g env e1, g env e2)
  | FNeg(e) -> FNeg(g env e)
  | FAdd(e1, e2) -> FAdd(g env e1, g env e2)
  | FSub(e1, e2) -> FSub(g env e1, g env e2)
  | FMul(e1, e2) -> FMul(g env e1, g env e2)
  | FDiv(e1, e2) -> FDiv(g env e1, g env e2)
  | Eq(e1, e2) -> Eq(g env e1, g env e2)
  | LE(e1, e2) -> LE(g env e1, g env e2)
  | If(e1, e2, e3) -> If(g env e1, g env e2, g env e3)
  | Let((x, [xt]), [e1], e2) ->
      let cnt = ref 0 in
      let e2' = g (M.add x cnt env) e2 in
      Let((x, copy_type_n !cnt xt), gn !cnt env e1, e2')
  | Var(x, _) ->
      if M.mem x env then
        let cnt = M.find x env in
        let cvalue = !cnt in
        cnt := cvalue + 1;
        Var(x, cvalue)
      else Var(x, 0) (* 外部変数または関数とその引数 *)
  | LetRec((lx, [lxt]), [{ name = (x, t); args = yts; body = e1 }], e2) ->
      (* 関数とその引数はコピーしない *)
      let benv = List.fold_left (fun env y -> M.remove y env) (M.remove x env) (List.map fst yts) in
      let cnt = ref 0 in
      let e2' = g (M.add lx cnt env) e2 in
      LetRec((lx, copy_type_n !cnt lxt),
             List.map (fun e1' -> { name = (x, copy_type t);
                                    args = List.map (fun (y, t) -> (y, copy_type t)) yts;
                                    body = e1' }) (gn !cnt benv e1),
              e2')
  | App(e1, e2s, t) -> App(g env e1, List.map (g env) e2s, copy_type t)
  | Tuple(es) -> Tuple(List.map (g env) es)
  | LetTuple(xtss, [e1], e2) ->
      let cnt = ref 0 in
      let e2' = g (M.add_list (List.map (fun (x, _) -> (x, cnt)) xtss) env) e2 in
      LetTuple(List.map (fun (x, [xt]) -> (x, copy_type_n !cnt xt)) xtss, gn !cnt env e1, e2')
  | Array(e1, e2) -> Array(g env e1, g env e2)
  | Get(e1, e2) -> Get(g env e1, g env e2)
  | Put(e1, e2, e3) -> Put(g env e1, g env e2, g env e3)
  | List(e1s) -> List(List.map (g env) e1s)
  | LAdd(e1, e2) -> LAdd(g env e1, g env e2)
  | Match([e1], e2, (x, [xt]), (y, [yt]), e3) ->
      let cnt = ref 0 in
      let e2' = g env e2 in
      let e3' = g (M.add_list [(x, cnt); (y, cnt)] env) e3 in
      Match(gn !cnt env e1, e2', (x, copy_type_n !cnt xt), (y, copy_type_n !cnt yt), e3')
  | _ -> failwith "unsupported input for AST duplicating"
and gn n env e =
  let rec gni n ret =
    if n <= 1 then (g env e) :: ret
              else gni (n - 1) ((g env e) :: ret)
  in gni n []

let f e =
  g M.empty e
