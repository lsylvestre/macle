open Err
open Loc
open Ast
open Types

exception Cannot_unify of ty * ty * loc
exception Unbound_value of ident * loc
exception Recursive_function_must_be_first_order of ident * ty * loc
exception Circuit_must_be_first_order of ty

let ty_of = TMACLE.ty_of

let rec unify loc env t1 t2 =
  (* List.iter (fun (x,t) -> Format.(fprintf std_formatter "(%s:%a)," x print_ty t)) env;
  Format.(fprintf std_formatter "unify [ %a | %a ]\n" Types.print_ty t1 Types.print_ty t2);
  (fun v -> Format.(fprintf std_formatter "---> [ %a | %a ]\n" Types.print_ty t1 Types.print_ty t2); v) @@ *)
  let t1 = canon t1
  and t2 = canon t2 in
  match t1,t2 with
  | TConst c1, TConst c2 when c1 = c2 ->
      ()
  | TConstr (x,tys), TConstr (x',tys') ->
      if x <> x' || List.compare_lengths tys tys' <> 0 then
        raise (Cannot_unify(t1,t2,loc));
      List.iter2 (unify loc env) tys tys'
  | TVar {contents=(V n)},
    TVar ({contents=V m} as v) ->
      if n = m then () else v := Ty t1
   | TVar ({contents=Ty t1'}),
     TVar ({contents=Ty t2'}) ->
       unify loc env t1' t2'
   | TVar ({contents=Ty t'} as v),t
   | t,TVar ({contents=Ty t'} as v) ->
       v := Ty t;
       unify loc env t' t
  | TVar ({contents=V n} as v),t
  | t,TVar ({contents=V n} as v) ->
      if occur n t then raise (Cannot_unify(t1,t2,loc));
      v := Ty t
  | TFun(ts1,t1),TFun(ts2,t2) ->
      if List.compare_lengths ts1 ts2 <> 0 then
        raise (Cannot_unify(t1,t2,loc));
      List.iter2 (unify loc env) ts1 ts2;
      unify loc env t1 t2
  | TFlatArray (t,z), TFlatArray (t',z') ->
      unify loc env t t';
      unify loc env z z'
  | TSize n, TSize n' when n = n' ->
      ()
  | _ ->
    raise (Cannot_unify(t1,t2,loc))

let typ_const c =
  match c with
  | Bool _ -> TConst TBool
  | Int _ -> TConst TInt
  | Unit -> TConst TUnit
  | Cstr _ -> failwith "todo"
  | EmptyList ->
      let v = newvar() in
      list_ v

let t_int = TConst TInt
let t_bool = TConst TBool
let t_unit = TConst TUnit

(* typing environment for operators *)
let typ_operator = function
  | `Binop (Add|Sub|Mul) ->
      TFun([t_int;t_int],t_int)
  | `Binop (Lt|Le|Gt|Ge) ->
      TFun([t_int;t_int],t_bool)
  | `Binop (Eq|Neq) ->
      TFun([t_int;t_int],t_bool)
  | `Unop Not ->
      TFun([t_bool],t_bool)
  | `Unop (Uminus|DivBy2|Mod2) ->
      TFun([t_int],t_int)


let first_order_side_condition t =
  let first_order = function
    | TFun _ -> false
    | _ -> true in
  match canon t with
  | TFun(tys,t) ->
      List.for_all first_order tys && first_order t
  | t ->
      first_order t

let typ_ident loc x env =
  match List.assoc_opt x env with
  | None ->
      raise (Unbound_value (x,loc))
  | Some t ->
      t

let typ_constr = function
  | "::" ->
      let v = newvar () in
      let ty = list_ v in
      ty,[v;ty]
  | "[]" ->
      let v = newvar () in
      list_ v,[]
  | "," ->
      let v = newvar () in
      let w = newvar () in
      TConstr("pair",[v;w]),[v;w]
  | x ->
      let tx,tys = typ_of_constructor x in
      TConstr(tx,[]), tys

let rec typ_exp (env : (ident * ty) list) (e,loc) =
  (fun (e,ty) -> e,canon ty) @@
  match e with
  | MACLE.Var x ->
      TMACLE.Var x, typ_ident loc x env
  | MACLE.Const c ->
      TMACLE.Const c, typ_const c
  | MACLE.If(e,e1,e2) ->
      let e' = typ_exp env e in
      unify loc env t_bool (ty_of e');
      let e1' = typ_exp env e1 in
      let e2' = typ_exp env e2 in
      let ty = ty_of e1' in
      unify loc env ty (ty_of e2');
      TMACLE.If(e',e1',e2'),ty
  | MACLE.Unop(p,e) ->
      let e' = typ_exp env e in
      let v = newvar() in
      unify loc env (TFun ([ty_of e'],v)) (typ_operator (`Unop p));
      TMACLE.Unop(p,e'),v
  | MACLE.Binop(p,e1,e2) ->
      let e1' = typ_exp env e1 in
      let e2' = typ_exp env e2 in
      let v = newvar() in
      unify loc env (TFun ([ty_of e1';ty_of e2'],v)) (typ_operator (`Binop p));
      TMACLE.Binop(p,e1',e2'),v
  | MACLE.CamlPrim(c) ->
      (match c with
       | MACLE.RefAccess(e) ->
           let v = newvar() in
           let e' = typ_exp env e in
           unify loc env (ty_of e') (ref_ v);
           TMACLE.CamlPrim(TMACLE.RefAccess e'),v
       | MACLE.RefAssign{r;e} ->
           let r' = typ_exp env r in
           let e' = typ_exp env e in
           unify loc env (ty_of r') (ref_ (ty_of e'));
           TMACLE.CamlPrim(TMACLE.RefAssign{r=r';e=e'}),t_unit
       | MACLE.ArrayAccess { arr ; idx } ->
           let arr' = typ_exp env arr in
           let idx' = typ_exp env idx in
           let v = newvar() in
           unify loc env (ty_of arr') (array_ v);
           unify loc env (ty_of idx') t_int;
           TMACLE.CamlPrim (TMACLE.ArrayAccess{arr=arr';idx=idx'}),v
       | MACLE.ArrayAssign { arr ; idx ; e} ->
           let arr' = typ_exp env arr in
           let idx' = typ_exp env idx in
           let e' = typ_exp env e in
           unify loc env (ty_of arr') (array_ (ty_of e'));
           unify loc env (ty_of idx') t_int;
           TMACLE.CamlPrim (TMACLE.ArrayAssign{arr=arr';idx=idx';e=e'}),t_unit
       | MACLE.ArrayLength e ->
           let e'= typ_exp env e in
           let v = newvar() in
           unify loc env (ty_of e') (array_ v);
           TMACLE.CamlPrim (TMACLE.ArrayLength e'),t_int)
  | MACLE.App(f,es) ->
      let tf = typ_ident loc f env in
      let tr =
        match tf with
        | TFun(tys,tr) -> tr
        | _ -> newvar()
      in
      let es' = List.map (typ_exp env) es in
      let tys' = List.map snd es' in
      let tf' = TFun(tys',tr) in
      unify loc env tf' tf;
      TMACLE.App(f,es'),tr
  | MACLE.LetFun(((f,xs),e1),e2) ->
      let tys = List.map (fun (x,_) -> newvar ()) xs in
      let bs = List.map2 (fun (x,_) ty -> x,ty) xs tys in
      let e1' =
        let env1 = bs @ env in
        typ_exp env1 e1
      in
      let ty = ty_of e1' in
      let e2' =
        let env2 = (f,TFun(tys,ty))::env in
       typ_exp env2 e2 in
      TMACLE.LetFun(((f,bs),e1'),e2'),ty_of e2'
  | MACLE.LetRec(ts,e) ->
      let env_ext = List.map (fun ((f,xs),_) -> f,List.map (fun _ -> newvar ()) xs,newvar ()) ts in
      let env' = List.map (fun (f,tys,ty) -> f, TFun (tys,ty)) env_ext@env in
      let ts' =
        List.map2 (
          fun ((f,xs),e) (_,tys,tr) ->
            let env'' = List.map2 (fun (x,_) ty -> x,ty) xs tys @ env' in
            let e' = typ_exp env'' e in
            unify loc env'' tr (ty_of e');
            ((f,List.map2 (fun (x,_) ty -> x,ty) xs tys),e')
        )
          ts env_ext in
      let e' = typ_exp env' e in
      List.iter (
        fun ((f,_),_) ->
          let tyf = typ_ident loc f env' in
          if not @@ first_order_side_condition tyf then
            raise (Recursive_function_must_be_first_order(f,tyf,loc))
      ) ts;
      TMACLE.LetRec(ts',e'),ty_of e'
  | MACLE.Let(bs,e) ->
      let typ_binding ((x,_),e) =
        let e' = typ_exp env e in
        ((x,ty_of e'),e')
      in
      let bs' = List.map typ_binding bs in
      let e' = typ_exp (List.map fst bs' @ env) e in
      TMACLE.Let(bs',e'),ty_of e'
  | MACLE.Match(e,cases) ->
      let e' = typ_exp env e in
      let v = newvar() in
      let typ_case (c,xs,e0) =
        match typ_constr c with
        | ty,tys ->
            unify loc env (ty_of e') ty;
            if List.compare_lengths xs tys <> 0 then
              assert false (* TODO arity constructor raise (Cannot_unify(t1,t2,loc)); *)
            else
              let xs' = List.map2 (fun (xopt,_) ty -> (xopt,ty)) xs tys in
              let extra_env = List.filter_map (function
                  | (Some x,ty) -> Some (x,ty)
                  | (None,_) -> None) xs' in
              let env' = extra_env @ env in
              let e0' = typ_exp env' e0 in
              unify loc env' v (ty_of e0');
              (c,xs',e0')
      in
      let cases' = List.map typ_case cases in
      TMACLE.Match(e',cases'),v
  | MACLE.Raise exc ->
      TMACLE.Raise(exc),newvar()
  | MACLE.FlatArrayOp c ->
      (match c with
       | FlatMake es ->
           let exp_of_typ ty e =
             let e' = typ_exp env e in
             unify loc env ty (ty_of e');
             e'
           in
           let v = newvar() in
           let es' = List.map (exp_of_typ v) es in
           let ty = TFlatArray (v,TSize (List.length es)) in
           TMACLE.(FlatArrayOp (FlatMake es'),ty)
       | FlatGet{e;idx} ->
           let e' = typ_exp env e in
           let idx' = typ_exp env idx in
           unify loc env t_int (ty_of idx');
           let ty = ty_of e' in
           let v = newvar() in
           let wsize = newvar() in
           unify loc env ty (TFlatArray(v,wsize));
           (* todo:check bounds in case of constant index *)
           TMACLE.(FlatArrayOp (FlatGet{e=e';idx=idx'}),v)
       | ArraySub(e,idx,n) ->
           let e' = typ_exp env e in
           let idx' = typ_exp env idx in
           unify loc env t_int (ty_of idx');
           let v = newvar() in
           unify loc env (ty_of e') (array_ v);
           TMACLE.(FlatArrayOp (ArraySub(e',idx',n)),(TFlatArray(v,TSize n)))
       | Map _ | Reduce _ -> assert false (* introduced later *)
      )
  | MACLE.Macro c ->
      (match c with
       | LazyOr(e1,e2) ->
           let e1' = typ_exp env e1 in
           let e2' = typ_exp env e2 in
           unify loc env (ty_of e1') t_bool;
           unify loc env (ty_of e2') t_bool;
           TMACLE.Macro(LazyOr(e1',e2')),t_bool
       | LazyAnd(e1,e2) ->
           let e1' = typ_exp env e1 in
           let e2' = typ_exp env e2 in
           unify loc env (ty_of e1') t_bool;
           unify loc env (ty_of e2') t_bool;
           TMACLE.Macro(LazyAnd(e1',e2')),t_bool
       | Map(f,es) ->
           let vs = List.map (fun _ -> newvar()) es in
           let wsize = newvar() in
           let es' = List.map (typ_exp env) es in
           let check_flat_array_of wsize e' ty : unit =
             unify loc env (flat_array_ ty wsize) (ty_of e')
           in
           List.iter2 (check_flat_array_of wsize) es' vs;
           let tf = typ_ident loc f env in
           let w = newvar() in
           unify loc env tf (TFun(vs,w));
           TMACLE.(Macro(Map(f,es')),flat_array_ w wsize)
       | Reduce(f,init,e) ->
           let v = newvar() in
           let wsize = newvar() in
           let e' = typ_exp env e in
           unify loc env (flat_array_ v wsize) (ty_of e');
           let init' = typ_exp env init in
           let tyr = ty_of init' in
           let tf = typ_ident loc f env in
           unify loc env tf (TFun([tyr;v],tyr));
           TMACLE.(Macro(Reduce(f,init',e')),tyr)
       | ArrayUpdate{arr;idx;e} ->
           let arr' = typ_exp env arr in
           let idx' = typ_exp env idx in
           unify loc env t_int (ty_of idx');
           let e' = typ_exp env e in
           let ty = ty_of arr' in
           let wsize = newvar() in
           unify loc env ty (TFlatArray(ty_of e',wsize));
           (* todo:check bounds in case of constant index *)
           TMACLE.(Macro(ArrayUpdate{arr=arr';idx=idx';e=e'})),ty
       | MACLE.OCamlArrayReduceBy(n,f,init,e) ->
           let v = newvar() in
           let e' = typ_exp env e in
           unify loc env (array_ v) (ty_of e');
           let init' = typ_exp env init in
           let tyr = ty_of init' in
           let tf = typ_ident loc f env in
           unify loc env tf (TFun([tyr;flat_array_ v (TSize n)],tyr));
           TMACLE.Macro(TMACLE.OCamlArrayReduceBy(n,f,init',e')),tyr
       | MACLE.OCamlArrayIterBy(n,f,e) ->
           let v = newvar() in
           let e' = typ_exp env e in
           unify loc env (array_ v) (ty_of e');
           let tf = typ_ident loc f env in
           unify loc env tf (TFun([flat_array_ v (TSize n)],t_unit));
           (* force la transformation à être de la forme 'a -> 'a,
                puisque le tableau est modifié en place *)
           TMACLE.Macro(TMACLE.OCamlArrayIterBy(n,f,e')),t_unit
       | MACLE.OCamlArrayMapBy(n,f,e) ->
           let v = newvar() in
           let e' = typ_exp env e in
           unify loc env (array_ v) (ty_of e');
           let tf = typ_ident loc f env in
           unify loc env tf (TFun([v],v));
           (* force la transformation à être de la forme 'a -> 'a,
              puisque le tableau est modifié en place *)
           TMACLE.Macro(TMACLE.OCamlArrayMapBy(n,f,e')),t_unit
       | MACLE.OCamlArrayFoldLeft(f,init,e) ->
           let v = newvar() in
           let e' = typ_exp env e in
           unify loc env (array_ v) (ty_of e');
           let init' = typ_exp env init in
           let tyr = ty_of init' in
           let tf = typ_ident loc f env in
           unify loc env tf (TFun([tyr;v],tyr));
           TMACLE.Macro(TMACLE.OCamlArrayFoldLeft(f,init',e')),tyr)


let rec canon_exp (desc,ty) =
  let open TMACLE in
  (fun desc' -> desc',canon ty) @@
  match desc with
    Var _ | Const _ -> desc
  | LetFun(((x,xs),e1),e2) ->
      let xs' = List.map (fun (x,ty) -> x, canon ty) xs in
      LetFun(((x,xs'),canon_exp e1),canon_exp e2)
  | LetRec(ts,e) ->
      let canon_fun_decl ((f,xs),e) =
        let xs' = List.map (fun (x,ty) -> (x,canon ty)) xs in
        let e' = canon_exp e in
        (f,xs'),e'
      in
      let ts' = List.map canon_fun_decl ts in
      let e' = canon_exp e in
      LetRec(ts',e')
  | If(e1,e2,e3) ->
      let e1' = canon_exp e1 in
      let e2' = canon_exp e2 in
      let e3' = canon_exp e3 in
      If(e1',e2',e3')
  | Unop(p,e) ->
      let e' = canon_exp e in
      Unop(p,e')
  | Binop(p,e1,e2) ->
      let e1' = canon_exp e1 in
      let e2' = canon_exp e2 in
      Binop(p,e1',e2')
  | App(q,es) ->
      App(q,List.map canon_exp es)
  | Let(bs,e) ->
      let canon_binding ((x,ty),e) =
        (x,canon ty), canon_exp e
      in
      let bs' = List.map canon_binding bs in
      let e' = canon_exp e in
      Let(bs',e')
  | Match(e,cases) ->
      let canon_case (c,xs,e) =
        let xs' = List.map (fun (x,ty) -> x,canon ty) xs in
        (c,xs', canon_exp e)
      in
      let cases' = List.map canon_case cases in
      Match(canon_exp e,cases')
  | Raise _ ->
      desc
  | CamlPrim(c) ->
      let c' =
          match c with
          | RefAccess e ->
              RefAccess(canon_exp e)
          | RefAssign{r;e} ->
              RefAssign{r=canon_exp r;e=canon_exp e}
          | ArrayAccess{arr;idx} ->
              ArrayAccess{arr=canon_exp arr;
                                idx=canon_exp idx}
          | ArrayAssign{arr;idx;e} ->
              ArrayAssign{arr=canon_exp arr;
                                idx=canon_exp idx;
                                e=canon_exp e}
          | ArrayLength e ->
              ArrayLength (canon_exp e)
       in
       CamlPrim c'
  | FlatArrayOp c ->
      FlatArrayOp
        (match c with
         | FlatMake es ->
             FlatMake (List.map canon_exp es)
         | FlatGet {e;idx} ->
             FlatGet {e = canon_exp e ; idx = canon_exp idx}
         | ArraySub(e,idx,n) ->
             ArraySub(canon_exp e,canon_exp idx,n)
         | Map(f,es) ->
             Map(f,List.map canon_exp es)
         | Reduce(f,init,e) ->
             Reduce(f,canon_exp init,canon_exp e)
        )
  | Macro c ->
      Macro
        (match c with
         | LazyOr(e1,e2) ->
             LazyOr(canon_exp e1,canon_exp e2)
         | LazyAnd(e1,e2) ->
             LazyAnd(canon_exp e1,canon_exp e2)
         | Map(f,es) ->
             Map(f,List.map canon_exp es)
         | Reduce (f,e1,e2) ->
             Reduce(f,canon_exp e1,canon_exp e2)
         | ArrayUpdate {arr;idx;e} ->
             ArrayUpdate {
               arr = canon_exp arr ;
               idx = canon_exp idx ;
               e = canon_exp e
             }
         | OCamlArrayReduceBy(n,f,init,e) ->
             OCamlArrayReduceBy(n,f,canon_exp init, canon_exp e)
         | OCamlArrayIterBy(n,f,e) ->
             OCamlArrayIterBy(n,f, canon_exp e)
         | OCamlArrayMapBy(n,f,e) ->
             OCamlArrayMapBy(n,f, canon_exp e)
         | OCamlArrayFoldLeft(f,e1,e2) ->
             OCamlArrayFoldLeft(f,canon_exp e1,canon_exp e2))


let typing_circuit MACLE.{x;xs;e;decoration=loc} =
  try
    let xs = List.map (fun (x,_) -> (x,newvar ())) xs in
    let e' = typ_exp xs e in

    let tyx = TFun(List.map snd xs,ty_of e') in
    if not (first_order_side_condition tyx) then
      raise @@ Circuit_must_be_first_order tyx;

    let xs = List.map (fun (x,t) -> (x,canon t)) xs in
    TMACLE.{x;xs;decoration=ty_of e';e=canon_exp e'}
  with
  | Unbound_value(y,loc) ->
      error x loc @@
      fun fmt () ->
      Format.fprintf fmt "Unbound value: %s" y
  | Cannot_unify(t1,t2,loc) ->
      error x loc @@
      fun fmt () ->
        Format.fprintf fmt "cannot unify types %a and %a."
          print_ty (canon t1)
          print_ty (canon t2)
  | Recursive_function_must_be_first_order(f,ty,loc) ->
      error x loc @@
        fun fmt () ->
          Format.fprintf fmt
            "Recursive functions must be first-order@,but %s has type %a." f
            print_ty (canon ty)
  | Circuit_must_be_first_order(ty) ->
      error x loc @@
        fun fmt () ->
          Format.fprintf fmt
            "Circuits must be first-order@,but %s has type %a." x
            print_ty (canon ty)
