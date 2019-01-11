open Syntax

module Tm =
  Map.Make
    (struct
      type t = Type.t
      let compare = compare
    end)

let func_type_cnt = ref 0
let func_type_map = ref Tm.empty
let func_type_id t = Tm.find t !func_type_map

let foldl1 f l = List.fold_left f (List.hd l) (List.tl l)

let rec merge_type t1 t2 = match t1, t2 with
    Type.Unit, Type.Unit | Type.Bool, Type.Bool | Type.Int, Type.Int | Type.Float, Type.Float -> t1
  | Type.Fun(a1s, r1), Type.Fun(a2s, r2) -> Type.Fun(List.map2 merge_type a1s a2s, merge_type r1 r2)
  | Type.Tuple(e1s), Type.Tuple(e2s) -> Type.Tuple(List.map2 merge_type e1s e2s)
  | Type.Array(e1), Type.Array(e2) -> Type.Array(merge_type e1 e2)
  | Type.List(e1), Type.List(e2) -> Type.List(merge_type e1 e2)
  | Type.Var(_), _ | _, Type.Var(_) -> assert false
  | _, _ -> Type.Multi

let rec merge_ast a1 a2 = match a1, a2 with
    Unit, Unit -> a1
  | Bool(v1), Bool(v2) when v1 = v2 -> a1
  | Int(v1), Int(v2) when v1 = v2 -> a1
  | Float(v1), Float(v2) when v1 = v2 -> a1
  | Not(e1), Not(e2) -> Not(merge_ast e1 e2)
  | Neg(e1), Neg(e2) -> Neg(merge_ast e1 e2)
  | Add(e11, e12), Add(e21, e22) -> Add(merge_ast e11 e21, merge_ast e12 e22)
  | Sub(e11, e12), Sub(e21, e22) -> Sub(merge_ast e11 e21, merge_ast e12 e22)
  | FNeg(e1), FNeg(e2) -> FNeg(merge_ast e1 e2)
  | FAdd(e11, e12), FAdd(e21, e22) -> FAdd(merge_ast e11 e21, merge_ast e12 e22)
  | FSub(e11, e12), FSub(e21, e22) -> FSub(merge_ast e11 e21, merge_ast e12 e22)
  | FMul(e11, e12), FMul(e21, e22) -> FMul(merge_ast e11 e21, merge_ast e12 e22)
  | FDiv(e11, e12), FDiv(e21, e22) -> FDiv(merge_ast e11 e21, merge_ast e12 e22)
  | Eq(e11, e12), Eq(e21, e22) -> Eq(merge_ast e11 e21, merge_ast e12 e22)
  | LE(e11, e12), LE(e21, e22) -> LE(merge_ast e11 e21, merge_ast e12 e22)
  | If(e11, e12, e13), If(e21, e22, e23) -> If(merge_ast e11 e21, merge_ast e12 e22, merge_ast e13 e23)
  | Let((x1, t1s), e11s, e12), Let((x2, t2s), e21s, e22) when x1 = x2 ->
      Let((x1, List.map2 merge_type t1s t2s), List.map2 merge_ast e11s e21s, merge_ast e12 e22)
  | Var(x1, _), Var(x2, _) when x1 = x2 -> Var(x1, 0)
  | LetRec((x1, t1s), def1s, e1), LetRec((x2, t2s), def2s, e2) when x1 = x2 ->
      LetRec((x1, t1s @ t2s), def1s @ def2s, merge_ast e1 e2)
  | App(e11, e12s, t1), App(e21, e22s, t2) when t1 = t2 ->
      App(merge_ast e11 e21, List.map2 merge_ast e12s e22s, merge_type t1 t2)
  | Tuple(e1s), Tuple(e2s) -> Tuple(List.map2 merge_ast e1s e2s)
  | LetTuple(xts1s, e11s, e12), LetTuple(xts2s, e21s, e22) ->
      LetTuple(List.map2 (fun (x, txs) (y, tys) -> assert (x = y);
                                                   (x, List.map2 merge_type txs tys)) xts1s xts2s,
               List.map2 merge_ast e11s e21s, merge_ast e12 e22)
  | Array(e11, e12), Array(e21, e22) -> Array(merge_ast e11 e21, merge_ast e12 e22)
  | Get(e11, e12), Get(e21, e22) -> Get(merge_ast e11 e21, merge_ast e12 e22)
  | Put(e11, e12, e13), Put(e21, e22, e23) -> Put(merge_ast e11 e21, merge_ast e12 e22, merge_ast e13 e23)
  | List(e1s), List(e2s) -> List(List.map2 merge_ast e1s e2s)
  | LAdd(e11, e12), LAdd(e21, e22) -> LAdd(merge_ast e11 e21, merge_ast e12 e22)
  | Match(e11s, e12, (x1, tx1s), (y1, ty1s), e13), Match(e21s, e22, (x2, tx2s), (y2, ty2s), e23)
    when x1 = x2 && y1 = y2 ->
      Match(List.map2 merge_ast e11s e21s, merge_ast e12 e22,
            (x1, List.map2 merge_type tx1s tx2s), (y1, List.map2 merge_type ty1s ty2s), merge_ast e13 e23)
  | _, _ -> assert false

let merge_types = foldl1 merge_type
let merge_asts = foldl1 merge_ast

let rec g e = match e with
    Unit -> Unit
  | Bool(v) -> Bool(v)
  | Int(v) -> Int(v)
  | Float(v) -> Float(v)
  | Not(e1) -> Not(g e1)
  | Neg(e1) -> Neg(g e1)
  | Add(e1, e2) -> Add(g e1, g e2)
  | Sub(e1, e2) -> Sub(g e1, g e2)
  | FNeg(e1) -> FNeg(g e1)
  | FAdd(e1, e2) -> FAdd(g e1, g e2)
  | FSub(e1, e2) -> FSub(g e1, g e2)
  | FMul(e1, e2) -> FMul(g e1, g e2)
  | FDiv(e1, e2) -> FDiv(g e1, g e2)
  | Eq(e1, e2) -> Eq(g e1, g e2)
  | LE(e1, e2) -> LE(g e1, g e2)
  | If(e1, e2, e3) -> If(g e1, g e2, g e3)
  | Let((x, xts), e1s, e2) -> Let((x, [merge_types xts]), [merge_asts e1s], g e2)
  | Var(x, n) -> Var(x, 0)
  | LetRec((x, xts), defs, e2) ->
      List.iter (fun t ->
                 if not (Tm.mem t !func_type_map)
                 then (func_type_map := Tm.add t !func_type_cnt !func_type_map;
                       func_type_cnt := !func_type_cnt + 1)) xts;
      LetRec((x, [merge_types xts]),
             List.map (fun { name = xt; args = yts; body = e1 } ->
                           { name = xt; args = yts; body = g e1 }) defs,
             g e2)
  | App(e1, e2s, t) ->
      if not (Tm.mem t !func_type_map)
        then (func_type_map := Tm.add t !func_type_cnt !func_type_map;
              func_type_cnt := !func_type_cnt + 1);
      App(g e1, List.map g e2s, t)
  | Tuple(e1s) -> Tuple(List.map g e1s)
  | LetTuple(xtss, e1s, e2) ->
      LetTuple(List.map (fun (x, ts) -> (x, [merge_types ts])) xtss, [merge_asts e1s], g e2)
  | Array(e1, e2) -> Array(g e1, g e2)
  | Get(e1, e2) -> Get(g e1, g e2)
  | Put(e1, e2, e3) -> Put(g e1, g e2, g e3)
  | List(e1s) -> List(List.map g e1s)
  | LAdd(e1, e2) -> LAdd(g e1, g e2)
  | Match(e1s, e2, (x, xts), (y, yts), e3) ->
      Match([merge_asts e1s], g e2, (x, [merge_types xts]), (y, [merge_types yts]), g e3)

let f e =
  func_type_cnt := 0;
  func_type_map := Tm.empty;
  g e
