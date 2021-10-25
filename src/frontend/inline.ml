open Ast
open TMACLE
open Types

(** systematically inline function calls.
    tail-recursive functions are translated into local loops.
    
    Non tail-recursive functions are not supported : 
    this silently generates incorrect code (to be improved !) *)

(* higher order functions that are not recursive are specialized *)

(** Inlining assume that each identifier binded by Let/LetFun/LetRec is unique:
    a renaming of identifiers must be perform on the source code beforehand. *)

let rec occur x e =
  match e with
  | Var x' ->
      x = x'
  | Const _ ->
      false
  | Prim (_,es) ->
      List.exists (occur x) es
  | If(e1,e2,e3,_) -> 
      occur x e1 || occur x e2 || occur x e3
  | Case(e1,_,hs,e2,_) -> 
      occur x e1 ||
      List.exists (fun (_,e) -> occur x e) hs ||
      occur x e2
  | Let(bs,e,_) -> 
      List.exists (fun (_,e) -> occur x e) bs ||
      (List.for_all (fun ((x',_),_) -> x <> x') bs && occur x e)
  | LetFun(((q,xs),e1),e2) -> 
      (List.for_all (fun (y,_) -> y <> x) xs && occur x e1) ||
      (x <> q && occur x e2)
  | LetRec(bs,e,_) -> 
      (List.for_all (fun ((x',xs),_) ->
                       x <> x' && List.for_all (fun (y,_) -> x <> y) xs) bs) && 
      (List.exists (fun ((_,_),e) -> occur x e) bs || occur x e)
  | App(x',es,_) -> 
      x' = x || List.exists (occur x) es
  | CamlPrim c -> 
      (match c with 
       | ArrayAccess{arr;idx} ->
           occur x arr || occur x idx
       | (RefAccess (e,_) | ArrayLength (e,_) | ListHd (e,_) | ListTl (e,_)) -> 
           occur x e
       | RefAssign{r;e} ->
           occur x r || occur x e
       | ArrayAssign{arr;idx;e} ->
           occur x arr || occur x idx || occur x e
       | (ListFoldLeft _ | ArrayFoldLeft _ | ArrayMapBy _) -> 
           assert false (* already expanded *) )


let env_extend bs env =
  List.map (fun ((x,xs),e) -> (x,(xs,e,bs))) bs @ env    


let substitution env e =
  let rec aux e = match e with
  | Var x -> (match List.assoc_opt x env with None -> e | Some e' -> e')
  | Const _ -> 
      e
  | Prim (p,es) ->
      Prim (p, List.map aux es)
  | If(e1,e2,e3,ty) -> 
      If(aux e1,aux e2,aux e3,ty)
  | Case(e1,ty,hs,e2,ty2) -> 
      Case(aux e1,ty,Misc.map_snd aux hs,aux e2,ty2)
  | Let(bs,e,ty) -> 
      Let(Misc.map_snd aux bs,aux e,ty)
  | LetFun((qxs,e1),e2) -> 
      LetFun((qxs,aux e1),aux e2)
  | LetRec(bs,e,ty) -> 
      LetRec(Misc.map_snd aux bs,aux e,ty)
  | App(x,es,ty) -> 
      let es' = List.map aux es in
      (match List.assoc_opt x env with 
       | None -> App(x,es',ty)
       | Some (Var q) -> App(q,List.map aux es,ty)
       | _ -> assert false)
  | CamlPrim c ->
      CamlPrim 
      (match c with 
       | ArrayAccess{arr;idx;ty} ->
           ArrayAccess{arr=aux arr;idx=aux idx;ty}
       | RefAccess (e,ty) ->
           RefAccess (aux e,ty)
       | ArrayLength (e,ty) ->
           ArrayLength (aux e,ty) 
       | ListHd (e,ty) -> 
           ListHd (aux e,ty)
       | ListTl (e,ty) -> 
           ListTl (aux e,ty)
       | RefAssign{r;e;ty} ->
            RefAssign{r=aux r;e= aux e;ty}
       | ArrayAssign{arr;idx;e;ty} ->
           ArrayAssign{arr= aux arr;idx= aux idx;e= aux e;ty}
       | (ListFoldLeft _ | ArrayFoldLeft _ | ArrayMapBy _) -> 
           assert false (* already expanded *) )
in aux e


let is_fun_type = function
| Types.TFun _ -> true
| _ -> false

let specialize_list xs es e =
  let bs = List.combine xs es in
  let env,bs = 
    List.partition_map (fun (((x,ty),e) as b) -> 
                         if is_fun_type ty then Left(x,e) 
                                           else Right b) bs in
  bs,substitution env e


let fetch x env =
  match List.assoc_opt x env with
  | None ->
      (* [x] doit être défini dans [env] 
         puisque le programme est supposé bien typé *)
    Printf.printf "** %s" x; 
    assert false
  | Some b ->
      b

let rec inline rec_env env e =
  match e with
  | (Var _ | Const _) -> e
  | Prim (c,es) ->
      Prim (c,List.map (inline rec_env env) es)
  | If(e1,e2,e3,ty) ->
      let e1' = inline rec_env env e1 in
      let e2' = inline rec_env env e2 in
      let e3' = inline rec_env env e3 in
      If(e1',e2',e3',ty)
  | Case(e1,ty,hs,e2,ty2) -> 
      let e1' = inline rec_env env e1 in
      let hs' = List.map (fun (c,e) -> c,inline rec_env env e) hs in
      let e2' = inline rec_env env e2 in
      Case(e1',ty,hs',e2',ty2)
  | Let(bs,e,ty) ->
      let bs' = List.map (fun (x,e) -> (x,inline rec_env env e)) bs in 
      let e' = inline rec_env env e in (* moins les xi *)
      mk_let ~ty bs' e'
  | LetFun(((x,args),e1),e2) ->
      (* [b] est "poussé" dans l'environnement *)
      (* renomme [x] au passage *) 
      let x' = Gensym.gensym x in
      let theta = [(x,Var x')] in
      let b' = ((x',args),substitution theta e1) in
      let env' = env_extend [b'] env in
      inline rec_env env' (substitution theta e2)
  | LetRec(bs,e,_) ->
      (* les [bs] sont "poussés" dans l'environnement *)
      (* renomme au passage les bs : c'est nécessaire en cas d'application
         d'une fonction [f] à une expression qui contient [f]. 
       *)
      let bs' = List.map (fun ((x,args),e) -> ((Gensym.gensym x,args),e)) bs in
      let env0 = List.map2 (fun ((x,_),e) ((x',_),_) -> (x,Var x')) bs bs' in
      let e = substitution env0 e in 
      let bs'' = List.map (fun ((x,args),e) ->((x,args),substitution env0 e)) bs' in
      let env' = env_extend bs'' env in
      inline rec_env env' e
  | App(x,es,ty) ->
      let es' = List.map (inline rec_env env) es in
      let (xs,e,bs) = fetch x env in
      if List.mem x rec_env then App(x,es',ty) (* déjà traité *) 
      else if List.exists (fun (_,e) -> occur x e) bs (* fonction récursive *)
           then let bs' = 
               List.map (fun ((y,xs'),e) ->
                 let rec_env' = List.map (fun ((x,_),_) -> x) bs @ rec_env in
                 (y,xs'),inline rec_env' env e) bs
             in
             LetRec(bs',App(x,es',ty),ty)
      else
        let bs',e0 = specialize_list xs es' e in
        let e' = inline rec_env env e0 in
        mk_let ~ty bs' e'
  | CamlPrim r -> 
    (match r with
    | RefAccess (e,t) -> 
        CamlPrim(RefAccess(inline rec_env env e,t))
    | RefAssign{r;e;ty} -> 
        CamlPrim(RefAssign{r=inline rec_env env r;
                           e=inline rec_env env e;ty})
    | ArrayAccess{arr;idx;ty} ->
        CamlPrim(ArrayAccess{arr=inline rec_env env arr;
                             idx=inline rec_env env idx;ty})
    | ArrayAssign{arr;idx;e;ty} ->
        CamlPrim(ArrayAssign{arr=inline rec_env env arr;
                             idx=inline rec_env env idx;
                             e=inline rec_env env e;ty})
    | ArrayLength (e,ty) -> 
        CamlPrim(ArrayLength(inline rec_env env e,ty))
    | ListHd (e,ty) -> 
        CamlPrim(ListHd(inline rec_env env e,ty))
    | ListTl (e,ty) -> 
        CamlPrim(ListTl(inline rec_env env e,ty))
    | (ListFoldLeft _ | ArrayFoldLeft _ | ArrayMapBy _) -> 
        assert false (* already expanded *) 
    )


let inline_circuit (c : TMACLE.circuit) : TMACLE.circuit = 
  {c with e = inline [] [] c.e}
