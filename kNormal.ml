(* give names to intermediate values (K-normalization) *)

module Tm =
  Map.Make
    (struct
      type t = Type.t
      let compare = compare
    end)

let funcTypeCnt = ref 0
let funcTypeMap = ref Tm.empty
let funcTypeId t = Tm.find t !funcTypeMap

type t = (* K正規化後の式 (caml2html: knormal_t) *)
  | Unit
  | Int of int
  | Float of float
  | Neg of Id.t
  | Add of Id.t * Id.t
  | Sub of Id.t * Id.t
  | FNeg of Id.t
  | FAdd of Id.t * Id.t
  | FSub of Id.t * Id.t
  | FMul of Id.t * Id.t
  | FDiv of Id.t * Id.t
  | IfEq of Id.t * Id.t * t * t (* 比較 + 分岐 (caml2html: knormal_branch) *)
  | IfLE of Id.t * Id.t * t * t (* 比較 + 分岐 *)
  | Let of (Id.t * Type.t) * t * t
  | Var of Id.t
  | LetRec of fundef list * t
  | App of Id.t * Id.t list
  | Tuple of Id.t list
  | LetTuple of (Id.t * Type.t) list * Id.t * t
  | Get of Id.t * Id.t
  | Put of Id.t * Id.t * Id.t
  | ExtArray of Id.t
  | ExtFunApp of Id.t * Id.t list
  | EmptyList
  | LAdd of Id.t * Id.t
  | Match of Id.t * t * (Id.t * Type.t) * (Id.t * Type.t) * t
and fundef = { name : Id.t * Type.t; args : (Id.t * Type.t) list; body : t }

let rec fv = function (* 式に出現する（自由な）変数 (caml2html: knormal_fv) *)
  | Unit | Int(_) | Float(_) | ExtArray(_) | EmptyList -> S.empty
  | Neg(x) | FNeg(x) -> S.singleton x
  | Add(x, y) | Sub(x, y) | FAdd(x, y) | FSub(x, y) | FMul(x, y) | FDiv(x, y) | Get(x, y) | LAdd(x, y) -> S.of_list [x; y]
  | IfEq(x, y, e1, e2) | IfLE(x, y, e1, e2) -> S.add x (S.add y (S.union (fv e1) (fv e2)))
  | Let((x, t), e1, e2) -> S.union (fv e1) (S.remove x (fv e2))
  | Var(x) -> S.singleton x
  | LetRec(defs, e2) ->
      let { name = (x, t); args = yts; body = e1 } = List.hd defs in
      let zs = S.diff (fv e1) (S.of_list (List.map fst yts)) in
      S.diff (S.union zs (fv e2)) (S.singleton x)
  | App(x, ys) -> S.of_list (x :: ys)
  | Tuple(xs) | ExtFunApp(_, xs) -> S.of_list xs
  | Put(x, y, z) -> S.of_list [x; y; z]
  | LetTuple(xs, y, e) -> S.add y (S.diff (fv e) (S.of_list (List.map fst xs)))
  | Match(x, e1, (y, yt), (z, zt), e2) ->
      let ws = S.diff (fv e2) (S.of_list [y; z]) in
      S.add x (S.union (fv e1) ws)

let insert_let (e, t) k = (* letを挿入する補助関数 (caml2html: knormal_insert) *)
  match e with
  | Var(x) -> k x
  | _ ->
      let x = Id.gentmp t in
      let e', t' = k x in
      Let((x, t), e, e'), t'

let rec transpose l = match l with
    []::_ -> []
  | l -> (List.map List.hd l) :: (transpose (List.map List.tl l))

let zip a b = List.map2 (fun x y -> (x, y)) a b

let rec g env = function (* K正規化ルーチン本体 (caml2html: knormal_g) *)
  | Syntax.Unit -> Unit, Type.Unit
  | Syntax.Bool(b) -> Int(if b then 1 else 0), Type.Int (* 論理値true, falseを整数1, 0に変換 (caml2html: knormal_bool) *)
  | Syntax.Int(i) -> Int(i), Type.Int
  | Syntax.Float(d) -> Float(d), Type.Float
  | Syntax.Not(e) -> g env (Syntax.If(e, Syntax.Bool(false), Syntax.Bool(true)))
  | Syntax.Neg(e) ->
      insert_let (g env e)
        (fun x -> Neg(x), Type.Int)
  | Syntax.Add(e1, e2) -> (* 足し算のK正規化 (caml2html: knormal_add) *)
      insert_let (g env e1)
        (fun x -> insert_let (g env e2)
            (fun y -> Add(x, y), Type.Int))
  | Syntax.Sub(e1, e2) ->
      insert_let (g env e1)
        (fun x -> insert_let (g env e2)
            (fun y -> Sub(x, y), Type.Int))
  | Syntax.FNeg(e) ->
      insert_let (g env e)
        (fun x -> FNeg(x), Type.Float)
  | Syntax.FAdd(e1, e2) ->
      insert_let (g env e1)
        (fun x -> insert_let (g env e2)
            (fun y -> FAdd(x, y), Type.Float))
  | Syntax.FSub(e1, e2) ->
      insert_let (g env e1)
        (fun x -> insert_let (g env e2)
            (fun y -> FSub(x, y), Type.Float))
  | Syntax.FMul(e1, e2) ->
      insert_let (g env e1)
        (fun x -> insert_let (g env e2)
            (fun y -> FMul(x, y), Type.Float))
  | Syntax.FDiv(e1, e2) ->
      insert_let (g env e1)
        (fun x -> insert_let (g env e2)
            (fun y -> FDiv(x, y), Type.Float))
  | Syntax.Eq _ | Syntax.LE _ as cmp ->
      g env (Syntax.If(cmp, Syntax.Bool(true), Syntax.Bool(false)))
  | Syntax.If(Syntax.Not(e1), e2, e3) -> g env (Syntax.If(e1, e3, e2)) (* notによる分岐を変換 (caml2html: knormal_not) *)
  | Syntax.If(Syntax.Eq(e1, e2), e3, e4) ->
      insert_let (g env e1)
        (fun x -> insert_let (g env e2)
            (fun y ->
              let e3', t3 = g env e3 in
              let e4', t4 = g env e4 in
              IfEq(x, y, e3', e4'), t3))
  | Syntax.If(Syntax.LE(e1, e2), e3, e4) ->
      insert_let (g env e1)
        (fun x -> insert_let (g env e2)
            (fun y ->
              let e3', t3 = g env e3 in
              let e4', t4 = g env e4 in
              IfLE(x, y, e3', e4'), t3))
  | Syntax.If(e1, e2, e3) -> g env (Syntax.If(Syntax.Eq(e1, Syntax.Bool(false)), e3, e2)) (* 比較のない分岐を変換 (caml2html: knormal_if) *)
  | Syntax.Let((x, ts), e1::_, e2) ->
      let e1', t1 = g env e1 in
      let e2', t2 = g (Vm.add_v x ts env) e2 in
      Let((x, List.hd ts), e1', e2'), t2
  | Syntax.Var(x, n) when Vm.mem (x, n) env -> Var(x), Vm.find (x, n) env
  | Syntax.Var(x, _) -> (* 外部配列の参照 (caml2html: knormal_extarray) *)
      (match M.find x !Typing.extenv with
      | Type.Array(_) as t -> ExtArray x, t
      | _ -> failwith (Printf.sprintf "external variable %s does not have an array type" x))
  | Syntax.LetRec((x, ts), defs, e2) ->
      let defs' = List.map (fun { Syntax.name = (x, t); Syntax.args = yts; Syntax.body = e1 } ->
        let env = Vm.add (x, 0) t env in
        let e1', _ = g (Vm.add_list (List.map (fun (y, t) -> ((y, 0), t)) yts) env) e1 in
        if not (Tm.mem t !funcTypeMap)
          then (funcTypeMap := Tm.add t !funcTypeCnt !funcTypeMap; funcTypeCnt := !funcTypeCnt + 1);
        { name = (x, t); args = yts; body = e1' }) defs in
      let env' = Vm.add_v x ts env in
      let e2', t2 = g env' e2 in
      LetRec(defs', e2'), t2
  | Syntax.App(Syntax.Var(f, n), e2s, _) when not (Vm.mem (f, n) env) -> (* 外部関数の呼び出し (caml2html: knormal_extfunapp) *)
      (match M.find f !Typing.extenv with
      | Type.Fun(_, t) ->
          let rec bind xs = function (* "xs" are identifiers for the arguments *)
            | [] -> ExtFunApp(f, xs), t
            | e2 :: e2s ->
                insert_let (g env e2)
                  (fun x -> bind (xs @ [x]) e2s) in
          bind [] e2s (* left-to-right evaluation *)
      | _ -> assert false)
  | Syntax.App(e1, e2s, _) ->
      (match g env e1 with
      | _, (Type.Fun(_, t) as ft) as g_e1 ->
          if not (Tm.mem ft !funcTypeMap)
            then (funcTypeMap := Tm.add ft !funcTypeCnt !funcTypeMap; funcTypeCnt := !funcTypeCnt + 1);
          insert_let g_e1
            (fun f ->
              let rec bind xs = function (* "xs" are identifiers for the arguments *)
                | [] -> App(f, xs), t
                | e2 :: e2s ->
                    insert_let (g env e2)
                      (fun x -> bind (xs @ [x]) e2s) in
              bind [] e2s) (* left-to-right evaluation *)
      | _ -> assert false)
  | Syntax.Tuple(es) ->
      let rec bind xs ts = function (* "xs" and "ts" are identifiers and types for the elements *)
        | [] -> Tuple(xs), Type.Tuple(ts)
        | e :: es ->
            let _, t as g_e = g env e in
            insert_let g_e
              (fun x -> bind (xs @ [x]) (ts @ [t]) es) in
      bind [] [] es
  | Syntax.LetTuple(xtss, e1::_, e2) ->
      insert_let (g env e1)
        (fun y ->
          let e2', t2 = g (List.fold_left (fun env (x, ts) -> Vm.add_v x ts env) env xtss) e2 in
          LetTuple(List.map (fun (x, ts) -> (x, List.hd ts)) xtss, y, e2'), t2)
  | Syntax.Array(e1, e2) ->
      insert_let (g env e1)
        (fun x ->
          let _, t2 as g_e2 = g env e2 in
          insert_let g_e2
            (fun y ->
              let l =
                match t2 with
                | Type.Float -> "create_float_array"
                | _ -> "create_array" in
              ExtFunApp(l, [x; y]), Type.Array(t2)))
  | Syntax.Get(e1, e2) ->
      (match g env e1 with
      |        _, Type.Array(t) as g_e1 ->
          insert_let g_e1
            (fun x -> insert_let (g env e2)
                (fun y -> Get(x, y), t))
      | _ -> assert false)
  | Syntax.Put(e1, e2, e3) ->
      insert_let (g env e1)
        (fun x -> insert_let (g env e2)
            (fun y -> insert_let (g env e3)
                (fun z -> Put(x, y, z), Type.Unit)))
  | Syntax.List([]) -> EmptyList, Type.List(Type.gentyp())
  | Syntax.List(e1 :: es) ->
      insert_let (g env (Syntax.List(es)))
        (fun y ->
          let _, t1 as g_e1 = g env e1 in
          insert_let g_e1
            (fun x -> LAdd(x, y), Type.List(t1)))
  | Syntax.LAdd(e1, e2) ->
      let _, t1 as g_e1 = g env e1 in
      insert_let g_e1
        (fun x -> insert_let (g env e2)
            (fun y -> LAdd(x, y), Type.List(t1)))
  | Syntax.Match(e1::_, e2, (x, txs), (y, tys), e3) ->
      insert_let (g env e1)
        (fun z ->
          let e2', t2 = g env e2 in
          let e3', t3 = g (Vm.add_list_v [(x, txs); (y, tys)] env) e3 in
          Match(z, e2', (x, List.hd txs), (y, List.hd tys), e3'), t2)

let f e = 
  funcTypeCnt := 0;
  funcTypeMap := Tm.empty;
  fst (g Vm.empty e)
