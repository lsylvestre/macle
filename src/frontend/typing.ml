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
  | TConst c1, TConst c2 when c1 = c2 -> ()
  | TPtr,TPtr -> ()
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
  | TFun(tys,t) -> List.for_all first_order tys && first_order t
  | t -> first_order t

let typ_ident loc x env = 
  match List.assoc_opt x env with
  | None -> raise (Unbound_value (x,loc))
  | Some t -> t

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
  | x -> let tx,tys = typ_of_constructor x in
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
         TMACLE.CamlPrim (TMACLE.ArrayLength e'),t_int
     | MACLE.ListHd e -> 
         let e' = typ_exp env e in
         let v = newvar() in
         unify loc env (ty_of e') (list_ v);
         TMACLE.CamlPrim (TMACLE.ListHd e'),v
     | MACLE.ListTl e -> 
         let e' = typ_exp env e in
         let v = newvar() in
         let te = ty_of e' in
         unify loc env te (list_ v);
         TMACLE.CamlPrim (TMACLE.ListTl e'),te
    | MACLE.ArrayMapBy(n,f,e) ->
         let v = newvar() in
         let e' = typ_exp env e in
         unify loc env (array_ v) (ty_of e');
         let tf = typ_ident loc f env in
         unify loc env tf (TFun([v],v));
         (* force la transformation à être de la forme 'a -> 'a,
            puisque le tableau est modifié en place *) 
         TMACLE.CamlPrim(TMACLE.ArrayMapBy(n,f,e')),t_unit
    | MACLE.ArrayFoldLeft(f,init,e) ->
       let v = newvar() in
       let e' = typ_exp env e in
       unify loc env (array_ v) (ty_of e');
       let init' = typ_exp env init in
       let tyr = ty_of init' in
       let tf = typ_ident loc f env in
       unify loc env tf (TFun([tyr;v],tyr));
       TMACLE.CamlPrim(TMACLE.ArrayFoldLeft(f,init',e')),tyr
    | MACLE.ListFoldLeft(f,init,e) ->
       let v = newvar() in
       let e' = typ_exp env e in
       unify loc env (list_ v) (ty_of e');
       let init' = typ_exp env init in
       let tyr = ty_of init' in
       let tf = typ_ident loc f env in
       unify loc env tf (TFun([tyr;v],tyr));
       TMACLE.CamlPrim(TMACLE.ListFoldLeft(f,init',e')),tyr)
  | MACLE.App(f,es) -> 
      let tf = typ_ident loc f env in
      let tr = match tf with 
              | TFun(tys,tr) -> tr 
              | _ -> newvar() in
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
      let ts' = List.map2 (fun ((f,xs),e) (_,tys,tr) -> 
                             let env'' = List.map2 (fun (x,_) ty -> x,ty) xs tys @ env' in  
                             let e' = typ_exp env'' e in
                             unify loc env'' tr (ty_of e');
                             ((f,List.map2 (fun (x,_) ty -> x,ty) xs tys),e')) ts env_ext in
      let e' = typ_exp env' e in
      
      List.iter 
            (fun ((f,_),_) -> 
                let tyf = typ_ident loc f env' in
                if not @@ first_order_side_condition tyf then
                raise (Recursive_function_must_be_first_order(f,tyf,loc)))
            ts;
      TMACLE.LetRec(ts',e'),ty_of e'
  | MACLE.Let(bs,e) ->
      let bs' = List.map (fun ((x,_),e) -> 
                           let e' = typ_exp env e in 
                           ((x,ty_of e'),e')) bs in
      let e' = typ_exp (List.map fst bs' @ env) e in
      TMACLE.Let(bs',e'),ty_of e'
  | MACLE.Match(e,cases) ->
      let e' = typ_exp env e in
      let v = newvar() in
      let cases' = List.map (fun (c,xs,e0) -> 
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
                                     (c,xs',e0')) cases in
      TMACLE.Match(e',cases'),v
  
  | MACLE.Raise exc ->
      TMACLE.Raise(exc),newvar()

let rec canon_exp (desc,ty) = 
  let open TMACLE in
  (fun desc' -> desc',canon ty) @@
  match desc with
    Var _ | Const _ -> desc
  | LetFun(((x,xs),e1),e2) ->
      let xs' = List.map (fun (x,ty) -> x, canon ty) xs in
      LetFun(((x,xs'),canon_exp e1),canon_exp e2) 
  | LetRec(ts,e) ->
      let ts' = List.map (fun ((f,xs),e) -> 
          (f,List.map (fun (x,ty) -> (x,canon ty)) xs),canon_exp e) ts in
        LetRec(ts',canon_exp e)
  | If(e1,e2,e3) -> 
      If(canon_exp e1,canon_exp e2,canon_exp e3)
  | Unop(p,e) -> 
      Unop(p,canon_exp e)
  | Binop(p,e1,e2) -> 
      Binop(p,canon_exp e1,canon_exp e2)
  | App(q,es) -> 
      App(q,List.map canon_exp es)
  | Let(bs,e) -> 
      Let(List.map (fun ((x,ty),e) -> (x,canon ty), canon_exp e) bs, 
          canon_exp e)
  | Match(e,cases) -> 
      let cases' = List.map (fun (c,xs,e) -> 
                        let xs' = List.map (fun (x,ty) -> x,canon ty) xs in
                        (c,xs', canon_exp e)) cases in
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
          | ListHd e ->
              ListHd (canon_exp e)
          | ListTl e ->
              ListTl (canon_exp e)
          | ArrayMapBy(n,q,e) ->
              ArrayMapBy(n,q, canon_exp e)
          | ArrayFoldLeft(q,e1,e2) ->
              ArrayFoldLeft(q,canon_exp e1,canon_exp e2)
          | ListFoldLeft(q,e1,e2) ->
              ListFoldLeft(q,canon_exp e1,canon_exp e2)
       in 
       CamlPrim c'



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
          Format.fprintf fmt "Recursive functions must be first-order@,but %s has type %a." f 
            print_ty (canon ty)
  | Circuit_must_be_first_order(ty) ->
      error x loc @@
        fun fmt () -> 
          Format.fprintf fmt "Circuits must be first-order@,but %s has type %a." x
            print_ty (canon ty)
