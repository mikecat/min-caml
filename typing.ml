(* type inference/reconstruction *)

open Syntax

exception Unify of Type.t * Type.t
exception Error of t * Type.t * Type.t

let extenv = ref M.empty

(* for pretty printing (and type normalization) *)
let rec deref_typ = function (* 型変数を中身でおきかえる関数 (caml2html: typing_deref) *)
  | Type.MFun(t1s, t2, us, f) ->
      let us' = List.sort_uniq compare (List.map (fun (ts, t) -> (List.map deref_typ ts, deref_typ t)) !us) in
      us := us';
      Type.Fun(List.map deref_typ t1s, deref_typ t2, us')
  | Type.Tuple(ts) -> Type.Tuple(List.map deref_typ ts)
  | Type.Array(t) -> Type.Array(deref_typ t)
  | Type.Var({ contents = None } as r) ->
      let w = Type.genwild () in
      Format.eprintf "uninstantiated type variable detected@.";
      r := Some(w);
      w
  | Type.List(t) -> Type.List(deref_typ t)
  | Type.Var({ contents = Some(t) } as r) ->
      let t' = deref_typ t in
      r := Some(t');
      t'
  | t -> t
let rec deref_id_typ deref_typ (x, t) = (x, deref_typ t)
let rec deref_term deref_typ = function
  | Not(e) -> Not(deref_term deref_typ e)
  | Neg(e) -> Neg(deref_term deref_typ e)
  | Add(e1, e2) -> Add(deref_term deref_typ e1, deref_term deref_typ e2)
  | Sub(e1, e2) -> Sub(deref_term deref_typ e1, deref_term deref_typ e2)
  | Eq(e1, e2) -> Eq(deref_term deref_typ e1, deref_term deref_typ e2)
  | LE(e1, e2) -> LE(deref_term deref_typ e1, deref_term deref_typ e2)
  | FNeg(e) -> FNeg(deref_term deref_typ e)
  | FAdd(e1, e2) -> FAdd(deref_term deref_typ e1, deref_term deref_typ e2)
  | FSub(e1, e2) -> FSub(deref_term deref_typ e1, deref_term deref_typ e2)
  | FMul(e1, e2) -> FMul(deref_term deref_typ e1, deref_term deref_typ e2)
  | FDiv(e1, e2) -> FDiv(deref_term deref_typ e1, deref_term deref_typ e2)
  | If(e1, e2, e3) -> If(deref_term deref_typ e1, deref_term deref_typ e2, deref_term deref_typ e3)
  | Let(xt, e1, e2) -> Let(deref_id_typ deref_typ xt, deref_term deref_typ e1, deref_term deref_typ e2)
  | LetRec({ name = xt; args = yts; body = e1 }, e2) ->
      LetRec({ name = deref_id_typ deref_typ xt;
               args = List.map (deref_id_typ deref_typ) yts;
               body = deref_term deref_typ e1 },
             deref_term deref_typ e2)
  | App(e, es) -> App(deref_term deref_typ e, List.map (deref_term deref_typ) es)
  | Tuple(es) -> Tuple(List.map (deref_term deref_typ) es)
  | LetTuple(xts, e1, e2) ->
      LetTuple(List.map (deref_id_typ deref_typ) xts, deref_term deref_typ e1, deref_term deref_typ e2)
  | Array(e1, e2) -> Array(deref_term deref_typ e1, deref_term deref_typ e2)
  | Get(e1, e2) -> Get(deref_term deref_typ e1, deref_term deref_typ e2)
  | Put(e1, e2, e3) -> Put(deref_term deref_typ e1, deref_term deref_typ e2, deref_term deref_typ e3)
  | List(e) -> List(List.map (deref_term deref_typ) e)
  | LAdd(e1, e2) -> LAdd(deref_term deref_typ e1, deref_term deref_typ e2)
  | Match(e1, e2, xt, yt, e3) ->
      Match(deref_term deref_typ e1, deref_term deref_typ e2,
            deref_id_typ deref_typ xt, deref_id_typ deref_typ yt, deref_term deref_typ e3)
  | e -> e

let rec occur r1 = function (* occur check (caml2html: typing_occur) *)
  | Type.MFun(t2s, t2, _, _) -> List.exists (occur r1) t2s || occur r1 t2
  | Type.Tuple(t2s) -> List.exists (occur r1) t2s
  | Type.Array(t2) | Type.List(t2) -> occur r1 t2
  | Type.Var(r2) when r1 == r2 -> true
  | Type.Var({ contents = None }) -> false
  | Type.Var({ contents = Some(t2) }) -> occur r1 t2
  | _ -> false

let rec unify t1 t2 = (* 型が合うように、型変数への代入をする (caml2html: typing_unify) *)
  match t1, t2 with
  | Type.Unit, Type.Unit | Type.Bool, Type.Bool | Type.Int, Type.Int | Type.Float, Type.Float -> ()
  | Type.MFun(t1s, t1', u1s, f1), Type.MFun(t2s, t2', u2s, f2) ->
      let env = ref [] in
      (try List.iter2 (fun x y -> unify (Type.copy env x) (Type.copy env y)) t1s t2s
      with Invalid_argument(_) -> raise (Unify(t1, t2)));
      unify (Type.copy env t1') (Type.copy env t2');
      List.iter !f1 !u2s;
      List.iter !f2 !u1s;
      let f1' = !f1 in
      let f2' = !f2 in
      f1 := (fun t -> f1' t; f2' t);
      f2 := (fun t -> f2' t; f1' t)
  | Type.Tuple(t1s), Type.Tuple(t2s) ->
      (try List.iter2 unify t1s t2s
      with Invalid_argument(_) -> raise (Unify(t1, t2)))
  | Type.Array(t1), Type.Array(t2) | Type.List(t1), Type.List(t2) -> unify t1 t2
  | Type.Var(r1), Type.Var(r2) when r1 == r2 -> ()
  | Type.Var({ contents = Some(t1') }), _ -> unify t1' t2
  | _, Type.Var({ contents = Some(t2') }) -> unify t1 t2'
  | Type.Var({ contents = None } as r1), _ -> (* 一方が未定義の型変数の場合 (caml2html: typing_undef) *)
      if occur r1 t2 then raise (Unify(t1, t2));
      r1 := Some(t2)
  | _, Type.Var({ contents = None } as r2) ->
      if occur r2 t1 then raise (Unify(t1, t2));
      r2 := Some(t1)
  | _, _ -> raise (Unify(t1, t2))

let rec adjust_mmatch = function
  | Type.MFun(t1s, t2, us, f) ->
      let copy (ts, t) =
          let env = ref [] in
          (List.map (Type.copy env) ts, Type.copy env t) in
      let unify2 (t1s, t1) (t2s, t2) =
          List.iter2 unify t1s t2s;
          unify t1 t2 in
      let t1s' = List.map adjust_mmatch t1s in
      let t2' = adjust_mmatch t2 in
      List.iter (fun t -> unify2 (copy (t1s', t2')) t) !us;
      us := List.map (fun (ts, t) -> (List.map adjust_mmatch ts, adjust_mmatch t)) !us;
      Type.MFun(t1s', t2', us, f)
  | Type.Tuple(ts) -> Type.Tuple(List.map adjust_mmatch ts)
  | Type.Array(t) -> Type.Array(adjust_mmatch t)
  | Type.List(t) -> Type.List(adjust_mmatch t)
  | Type.Var({ contents = Some(t') } as r) as t ->
      r := Some(adjust_mmatch t');
      t
  | t -> t

let rec g env e = (* 型推論ルーチン (caml2html: typing_g) *)
  try
    match e with
    | Unit -> Type.Unit
    | Bool(_) -> Type.Bool
    | Int(_) -> Type.Int
    | Float(_) -> Type.Float
    | Not(e) ->
        unify Type.Bool (g env e);
        Type.Bool
    | Neg(e) ->
        unify Type.Int (g env e);
        Type.Int
    | Add(e1, e2) | Sub(e1, e2) -> (* 足し算（と引き算）の型推論 (caml2html: typing_add) *)
        unify Type.Int (g env e1);
        unify Type.Int (g env e2);
        Type.Int
    | FNeg(e) ->
        unify Type.Float (g env e);
        Type.Float
    | FAdd(e1, e2) | FSub(e1, e2) | FMul(e1, e2) | FDiv(e1, e2) ->
        unify Type.Float (g env e1);
        unify Type.Float (g env e2);
        Type.Float
    | Eq(e1, e2) | LE(e1, e2) ->
        unify (g env e1) (g env e2);
        Type.Bool
    | If(e1, e2, e3) ->
        unify (g env e1) Type.Bool;
        let t2 = g env e2 in
        let t3 = g env e3 in
        unify t2 t3;
        t2
    | Let((x, t), e1, e2) -> (* letの型推論 (caml2html: typing_let) *)
        unify t (g env e1);
        g (M.add x t env) e2
    | Var(x) when M.mem x env -> M.find x env (* 変数の型推論 (caml2html: typing_var) *)
    | Var(x) when M.mem x !extenv -> M.find x !extenv
    | Var(x) -> (* 外部変数の型推論 (caml2html: typing_extvar) *)
        Format.eprintf "free variable %s assumed as external@." x;
        let t = Type.gentyp () in
        extenv := M.add x t !extenv;
        t
    | LetRec({ name = (x, t); args = yts; body = e1 }, e2) -> (* let recの型推論 (caml2html: typing_letrec) *)
        let env = M.add x t env in
        let l = ref [] in
        let tenv = ref [] in
        let ts' = List.map (fun xt -> Type.Var({ contents = Some(snd xt) })) yts in
        let t' = Type.Var({ contents = Some(g (M.add_list yts env) e1) }) in
        unify t (Type.MFun(ts', t', l, ref (fun ts -> l := ts :: !l)));
        let rec copy_general = function
            | Type.MFun(gts, gt, _, _) ->
                let tenv = ref [] in
                let copy_type = function
                    | Type.Var({ contents = Some(t) } as tr) -> tr := Some(Type.copy tenv t)
                    | Type.Var({ contents = None }) -> ()
                    | _ -> Format.eprintf "cannot modify type to copy in let rec@.";
                           assert false in
                List.iter copy_type gts;
                copy_type gt
            | Type.Var({ contents = Some(t) }) -> copy_general t
            | _ ->
                Format.eprintf "new symbol in let rec is not a function@.";
                assert false in
        copy_general t;
        g env e2
    | App(e, es) -> (* 関数適用の型推論 (caml2html: typing_app) *)
        let t = Type.gentyp () in
        let ts = List.map (g env) es in
        let rec unify_app = function
            | Type.MFun(ats, rt, us, f) ->
                let tenv = ref [] in
                let ats' = List.map (Type.copy tenv) ats in
                let rt' = Type.copy tenv rt in
                List.iter2 unify ts ats';
                unify t rt';
                !f (ts, t)
            | Type.Var({ contents = Some(t') }) -> unify_app t'
            | Type.Var({ contents = None } as tr) ->
                let tenv = ref [] in
                let l = ref [(ts, t)] in
                let ts' = List.map (fun t -> Type.Var({ contents = Some(t) })) ts in
                tr := Some(Type.MFun(ts', Type.Var({ contents = Some(t) }), l, ref (fun ts -> l := ts :: !l)))
            | _ ->
                Format.eprintf "tried to apply what is not a function@.";
                assert false in
        unify_app (g env e);
        t
    | Tuple(es) -> Type.Tuple(List.map (g env) es)
    | LetTuple(xts, e1, e2) ->
        unify (Type.Tuple(List.map snd xts)) (g env e1);
        g (M.add_list xts env) e2
    | Array(e1, e2) -> (* must be a primitive for "polymorphic" typing *)
        unify (g env e1) Type.Int;
        Type.Array(g env e2)
    | Get(e1, e2) ->
        let t = Type.gentyp () in
        unify (Type.Array(t)) (g env e1);
        unify Type.Int (g env e2);
        t
    | Put(e1, e2, e3) ->
        let t = g env e3 in
        unify (Type.Array(t)) (g env e1);
        unify Type.Int (g env e2);
        Type.Unit
    | List(es) ->
        let t = Type.gentyp() in
        List.iter (function e -> unify t (g env e)) es;
        Type.List(t)
    | LAdd(e1, e2) ->
        let t = g env e1 in
        unify (Type.List(t)) (g env e2);
        Type.List(t)
    | Match(e1, e2, (x, tx), (y, ty), e3) ->
        unify (Type.List(tx)) ty;
        unify (g env e1) ty;
        let t2 = g env e2 in
        let t3 = g (M.add_list [(x, tx); (y, ty)] env) e3 in
        unify t2 t3;
        t2
  with Unify(t1, t2) -> raise (Error(deref_term deref_typ e, deref_typ t1, deref_typ t2))

let f e =
  extenv := M.empty;
(*
  (match deref_typ (g M.empty e) with
  | Type.Unit -> ()
  | _ -> Format.eprintf "warning: final result does not have type unit@.");
*)
  (try unify Type.Unit (g M.empty e)
  with Unify _ -> failwith "top level does not have type unit");
  let e' = deref_term adjust_mmatch e in
  extenv := M.map deref_typ !extenv;
  deref_term deref_typ e'
