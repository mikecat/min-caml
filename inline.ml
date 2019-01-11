open KNormal

(* インライン展開する関数の最大サイズ (caml2html: inline_threshold) *)
let threshold = ref 0 (* Mainで-inlineオプションによりセットされる *)

let rec size = function
  | IfEq(_, _, e1, e2) | IfLE(_, _, e1, e2)
  | Let(_, e1, e2) | LetRec(_, { body = e1 }::_, e2) -> 1 + size e1 + size e2
  | LetTuple(_, _, e) -> 1 + size e
  | _ -> 1

let rec g env = function (* インライン展開ルーチン本体 (caml2html: inline_g) *)
  | IfEq(x, y, e1, e2) -> IfEq(x, y, g env e1, g env e2)
  | IfLE(x, y, e1, e2) -> IfLE(x, y, g env e1, g env e2)
  | Let(xt, e1, e2) -> Let(xt, g env e1, g env e2)
  | LetRec(xt, defs, e2) -> (* 関数定義の場合 (caml2html: inline_letrec) *)
      let env = List.fold_left (fun env { name = (x, t); args = yts; body = e1 } ->
        if size e1 > !threshold then env else Vm.add (x, Merge.func_type_id t) (yts, e1) env) env defs in
      LetRec(xt, List.map (fun { name = xt; args = yts; body = e1 } ->
                               { name = xt; args = yts; body = g env e1}) defs, g env e2)
  | App(x, ys, n) when Vm.mem (x, n) env -> (* 関数適用の場合 (caml2html: inline_app) *)
      let (zs, e) = Vm.find (x, n) env in
      Format.eprintf "inlining %s@." x;
      let env' =
        List.fold_left2
          (fun env' (z, t) y -> M.add z y env')
          M.empty
          zs
          ys in
      Alpha.g env' e
  | LetTuple(xts, y, e) -> LetTuple(xts, y, g env e)
  | Match(x, e1, y, z, e2) -> Match(x, g env e1, y, z, g env e2)
  | e -> e

let f e = g Vm.empty e
