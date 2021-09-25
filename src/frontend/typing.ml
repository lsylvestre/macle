open Loc
open Ast
open Types

exception Cannot_unify of ty * ty * loc
exception Unbound_state of state
exception Bad_arity_op of (Atom.op * int * int)

let rec unify loc env t1 t2 =
  (* List.iter (fun (x,t) -> Format.(fprintf std_formatter "(%s:%a)," x print_ty t)) env;
  Format.(fprintf std_formatter "unify [ %a | %a ]\n" Types.print_ty t1 Types.print_ty t2); 
  (fun v -> Format.(fprintf std_formatter "---> [ %a | %a ]\n" Types.print_ty t1 Types.print_ty t2); v) @@ *)
  let t1 = canon t1 
  and t2 = canon t2 in
  match t1,t2 with
  | TConst c1, TConst c2 when c1 = c2 -> ()
  | TPtr,TPtr -> ()
  | TCamlRef t, TCamlRef t'
  | TCamlArray t, TCamlArray t' 
  | TCamlList t, TCamlList t' -> 
      unify loc env t t'
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

let typ_var x env = 
   match List.assoc_opt x env with
   | None -> failwith ("unbound variable "^x)
   | Some t -> t

let typ_const c = 
  let open Atom in
  match c with
  | Std_logic _ -> TConst TStd_logic
  | Bool _ -> TConst TBool 
  | Int _ -> TConst TInt
  | Unit -> TConst TUnit
  | EmptyList -> 
     let v = newvar() in 
     TCamlList v

let t_int = TConst TInt
let t_bool = TConst TBool
let t_unit = TConst TUnit

let typ_prim p =
  let open Atom in
  match p with
  | Binop c -> 
    (match c with 
    | (Add|Sub|Mul) -> TFun([t_int;t_int],t_int)
    | (Lt|Le|Gt|Ge) -> TFun([t_int;t_int],t_bool)
    | (Eq|Neq) ->TFun([t_int;t_int],t_bool)
    | (And|Or) -> TFun([t_bool;t_bool],t_bool))
  | Unop c -> 
    (match c with
     | Not -> TFun([t_bool],t_bool)
     | (Uminus|DivBy2|Mod2) -> TFun([t_int],t_int))
  | _ -> assert false

let typ_state q env = 
  match List.assoc_opt q env with
  | None -> raise (Unbound_state q)
  | Some t -> t

let rec typ_exp (env : (ident * ty) list) (e,loc) =
  (fun (ty,e) -> canon ty,e) @@
  match e with
  | MACLE.Var x -> 
      typ_var x env,TMACLE.Var x
  | MACLE.Const c -> 
      typ_const c,TMACLE.Const c
  | MACLE.If(e,e1,e2) -> 
      let ty,e' = typ_exp env e in
      unify loc env t_bool ty;
      let ty1,e1' = typ_exp env e1 in
      let ty2,e2' = typ_exp env e2 in
      unify loc env ty1 ty2;
      ty1,TMACLE.If(e',e1',e2',ty1)
  | MACLE.Case(e1,handlers,e2) -> 
      let ty1,e1' = typ_exp env e1 in
      let ty2,e2' = typ_exp env e2 in
      let hs = List.map (fun (c,e) -> 
                       let ty',e' = typ_exp env e in
                       unify loc env ty1 (typ_const c);
                       unify loc env ty2 ty';
                       (c,e')) handlers in
      ty2,TMACLE.Case(e1',ty1,hs,e2',ty2)
  | MACLE.Prim((Binop (Eq|Neq)) as p,[e1;e2]) -> 
      let ty1,e1' = typ_exp env e1 in
      let ty2,e2' = typ_exp env e2 in
      unify loc env ty1 ty2;
      t_bool,TMACLE.Prim(p,[e1';e2'])
  (* | MACLE.Prim(TyAnnot ty,[e]) ->
      let ty',e' = typ_exp env e in
      unify loc env ty ty';
      ty,e' *)
  | MACLE.Prim(p,es) -> 
      (match typ_prim p with
      | TFun(tys,tr) ->
          let tys',es' = Misc.split_map (typ_exp env) es in
          if List.compare_lengths tys tys' <> 0 then 
            raise (Bad_arity_op(p,List.length tys,List.length tys'));
          List.iter2 (unify loc env) tys tys';
          tr,TMACLE.Prim(p,es')
       | _ -> assert false)
  | MACLE.CamlPrim(c) -> 
     (match c with
     | MACLE.RefAccess(e) ->
         let v = newvar() in
         let ty,e' = typ_exp env e in
         unify loc env ty (TCamlRef v);
         v,TMACLE.CamlPrim(TMACLE.RefAccess (e',v))
     | MACLE.RefAssign{r;e} ->
        let tyr,r' = typ_exp env r in
        let tye,e' = typ_exp env e in
        unify loc env tyr (TCamlRef tye);
        t_unit,TMACLE.CamlPrim(TMACLE.RefAssign{r=r';e=e';ty=tye})
     | MACLE.ArrayAccess { arr ; idx } ->
        let (t,arr') = typ_exp env arr in
        let (tidx,idx') = typ_exp env idx in
        let v = newvar() in
        unify loc env t (TCamlArray v);
        unify loc env tidx t_int;
        v,TMACLE.CamlPrim (TMACLE.ArrayAccess{arr=arr';idx=idx';ty=v})
     | MACLE.ArrayAssign { arr ; idx ; e} -> 
        let (t,arr') = typ_exp env arr in
        let (tidx,idx') = typ_exp env idx in
        let (te,e') = typ_exp env e in
        unify loc env t (TCamlArray te);
        unify loc env tidx t_int;
        t_unit,TMACLE.CamlPrim (TMACLE.ArrayAssign{arr=arr';idx=idx';e=e';ty=te})
     | MACLE.ArrayLength e -> 
         let (te,e') = typ_exp env e in
         let v = newvar() in
         unify loc env te (TCamlArray v);
         t_int,TMACLE.CamlPrim (TMACLE.ArrayLength (e',v))
     | MACLE.ListHd(e) -> 
         let (te,e') = typ_exp env e in
         let v = newvar() in
         unify loc env te (TCamlList v);
         v,TMACLE.CamlPrim (TMACLE.ListHd (e',v))
     | MACLE.ListTl(e) -> 
         let (te,e') = typ_exp env e in
         let v = newvar() in
         unify loc env te (TCamlList v);
         te,TMACLE.CamlPrim (TMACLE.ListTl (e',v))
    | MACLE.ArrayMapBy(n,q,e) ->
         let v = newvar() in
         let ty,e' = typ_exp env e in
         unify loc env (TCamlArray v) ty;
         let tf = typ_state q env in
         unify loc env tf (TFun([v],v));
         (* force la transformation à être de la forme 'a -> 'a,
            puisque le tableau est modifié en place *) 
         t_unit,TMACLE.CamlPrim(TMACLE.ArrayMapBy(n,q,v,e'))
    | MACLE.ArrayFoldLeft(q,init,e) ->
       let v = newvar() in
       let ty,e' = typ_exp env e in
       unify loc env (TCamlArray v) ty;
       let tyr,init' = typ_exp env init in
       let tf = typ_state q env in
       unify loc env tf (TFun([tyr;v],tyr));
       tyr,TMACLE.CamlPrim(TMACLE.ArrayFoldLeft(q,v,tyr,init',e'))
    | MACLE.ListFoldLeft(q,init,e) ->
       let v = newvar() in
       let ty,e' = typ_exp env e in
       unify loc env (TCamlList v) ty;
       let tyr,init' = typ_exp env init in
       let tf = typ_state q env in
       unify loc env tf (TFun([tyr;v],tyr));
       tyr,TMACLE.CamlPrim(TMACLE.ListFoldLeft(q,v,tyr,init',e')))
  | MACLE.App(q,es) -> 
      let tf = typ_state q env in
      let tr = match tf with 
              | TFun(tys,tr) -> tr 
              | _ -> newvar() 
      in
      let tys',es' = Misc.split_map (typ_exp env) es in
      let tf' = TFun(tys',tr) in
      unify loc env tf' tf;
      tr,TMACLE.App(q,es',tr)
  | MACLE.LetFun(((q,xs),e1),e2) ->
    let tys = List.map (fun x -> newvar ()) xs in
    let bs = List.combine xs tys in
    let ty,e1' =
      let env1 = bs @ env in
      typ_exp env1 e1 
    in
    let ty2,e2' = 
       let env2 = (q,TFun(tys,ty))::env in
       typ_exp env2 e2 in
    ty2,TMACLE.LetFun(((q,bs),e1'),e2')
  | MACLE.LetRec(ts,e) ->
      let env_ext = List.map (fun ((q,xs),_) -> q,List.map (fun _ -> newvar ()) xs,newvar ()) ts in
      let env' = List.map (fun (q,tys,ty) -> q,TFun (tys,ty)) env_ext@env in
      let ts' = List.map2 (fun ((q,xs),e) (q,tys,tr) -> 
                             let env'' = List.combine xs tys @ env' in  
                             let t,e' = typ_exp env'' e in
                             unify loc env'' tr t;
                             ((q,List.combine xs tys),e')) ts env_ext in
      let ty,e' = typ_exp env' e in
      ty,TMACLE.LetRec(ts',e')
  | MACLE.Let(bs,e) ->
      let bs' = List.map (fun (x,e) -> 
                           let ty,e' = typ_exp env e in 
                           ((x,ty),e')) bs in
      let ty,e' = typ_exp (List.map fst bs'@env) e in
      ty,TMACLE.Let(bs',e',ty)

let rec canon_exp e = 
  let open TMACLE in
  match e with
    Var _ | Const _ -> e
  | LetFun(((x,xs),e1),e2) ->
      let xs' = List.map (fun (x,ty) -> x, canon ty) xs in
      LetFun(((x,xs'),canon_exp e1),canon_exp e2) 
  | LetRec(ts,e) ->
      let ts' = List.map (fun ((q,xs),e) -> 
          (q,List.map (fun (x,ty) -> (x,canon ty)) xs),canon_exp e) ts in
        LetRec(ts',canon_exp e)
  | If(e1,e2,e3,ty) -> 
      If(canon_exp e1,canon_exp e2,canon_exp e3,canon ty)
  | Case(e1,ty,hs,e2,ty2) -> 
      Case(canon_exp e1,
           canon ty,
           List.map (fun (c,e) -> c, canon_exp e) hs,
           canon_exp e2,
           canon ty2)
  | Prim (p,es) -> 
      Prim (p,List.map canon_exp es)
  | App(q,es,ty) -> 
      App(q,List.map canon_exp es,canon ty)
  | Let(bs,e,ty) -> 
      Let(List.map (fun ((x,ty),e) -> (x,canon ty), canon_exp e) bs, 
          canon_exp e,
          canon ty)
  | CamlPrim(c) ->
      let c' = 
          match c with
          | RefAccess(e,t) -> 
              RefAccess(canon_exp e,canon t)
          | RefAssign{r;e;ty} ->
              RefAssign{r=canon_exp r;e=canon_exp e;ty=canon ty}
          | ArrayAccess{arr;idx;ty} ->
              ArrayAccess{arr=canon_exp arr;
                                idx=canon_exp idx;
                                ty=canon ty} 
          | ArrayAssign{arr;idx;e;ty} ->
              ArrayAssign{arr=canon_exp arr;
                                idx=canon_exp idx;
                                e=canon_exp e;
                                ty=canon ty} 
          | ArrayLength (e,ty) ->
              ArrayLength (canon_exp e,canon ty)
          | ListHd (e,ty) ->
              ListHd (canon_exp e,canon ty)
          | ListTl (e,ty) ->
              ListTl (canon_exp e,canon ty)
          | ArrayMapBy(n,q,ty,e) ->
              ArrayMapBy(n,q,canon ty, canon_exp e)
          | ArrayFoldLeft(q,ty,tyr,e1,e2) ->
              ArrayFoldLeft(q,canon ty,canon tyr,canon_exp e1,canon_exp e2)
          | ListFoldLeft(q,ty,tyr,e1,e2) ->
              ListFoldLeft(q,canon ty,canon tyr,canon_exp e1,canon_exp e2)
       in 
       CamlPrim c'


let typing_circuit MACLE.{x;xs;e;loc} =
  try 
    let xs = List.map (fun x -> (x,newvar ())) xs in
    let ty,e' = typ_exp xs e in
    let xs = List.map (fun (x,t) -> (x,canon t)) xs in
    TMACLE.{x;xs;s=[];ty;e=canon_exp e'}
  with
  | Unbound_state q -> 
      error x loc @@
      fun fmt () -> 
      Format.fprintf fmt "unbound state:%s" q
  | Bad_arity_op (p,expected,n) -> 
     error x loc @@
     fun fmt () -> 
     Format.fprintf fmt 
       "Primitive %a as %d arguments but %d arguments were extected." 
         Pprint_ast.PP_atom.pp_op p n expected
  | Cannot_unify (t1,t2,loc) ->
      error x loc @@
      fun fmt () -> 
        Format.fprintf fmt "cannot unify types %a and %a." 
           print_ty t1 
           print_ty t2
