open Ast.TMACLE

(** constant/copy propagation 
    e.g. [let x = y and z = 2 in x + z + 1] ~> [y + 2 + 1] *)

(** Assume that each identifier binded by a Let is unique:
    a renaming of identifiers must be perform on the source code beforehand. *)

let propagation e =
  let rec aux env e = match e with
  | Var x -> (match List.assoc_opt x env with None -> e | Some e' -> e')
  | Const _ -> 
      e
  | Prim (p,es) ->
      Prim (p, List.map (aux env) es)
  | If(e1,e2,e3,ty) -> 
      If(aux env e1,aux env e2,aux env e3,ty)
  | Case(e1,ty,hs,e2,ty2) -> 
      Case(aux env e1,ty,Misc.map_snd (aux env) hs,aux env e2,ty2)
  | Let(bs,e,ty) ->
      let bs' = Misc.map_snd (aux env) bs in
      let env_ext,bs = List.partition_map 
        (function 
         | ((x,_),((Var _ | Const _) as e)) -> Left (x,e) 
         | b -> Right b) bs' 
      in
      mk_let bs (aux (env_ext@env) e) ty
  | LetFun((qxs,e1),e2) -> 
      LetFun((qxs,aux env e1),aux env e2)
  | LetRec(bs,e) -> 
      LetRec(Misc.map_snd (aux env) bs,aux env e)
  | App(x,es,ty) -> 
      let es' = List.map (aux env) es in
      (match List.assoc_opt x env with 
       | None -> App(x,es',ty)
       | Some (Var q) -> App(q,List.map (aux env) es,ty)
       | _ -> assert false)
  | CamlPrim c ->
      CamlPrim 
      (match c with 
       | ArrayAccess{arr;idx;ty} ->
           ArrayAccess{arr=aux env arr;idx=aux env idx;ty}
       | RefAccess (e,ty) ->
           RefAccess (aux env e,ty)
       | ArrayLength (e,ty) ->
           ArrayLength (aux env e,ty) 
       | ListHd (e,ty) -> 
           ListHd (aux env e,ty)
       | ListTl (e,ty) -> 
           ListTl (aux env e,ty)
       | RefAssign{r;e;ty} ->
            RefAssign{r=aux env r;e= aux env e;ty}
       | ArrayAssign{arr;idx;e;ty} ->
           ArrayAssign{arr= aux env arr;idx= aux env idx;e= aux env e;ty}
       | ListFoldLeft(q,ty1,ty2,init,e) ->
           ListFoldLeft(q,ty1,ty2,aux env init,aux env e) 
       | ArrayFoldLeft(q,ty1,ty2,init,e) ->
           ArrayFoldLeft(q,ty1,ty2,aux env init,aux env e)
       | ArrayMapBy(n,q,ty,e) ->
           ArrayMapBy(n,q,ty,aux env e))
in aux [] e


let constant_copy_propagation (c : circuit) : circuit = 
  {c with e = propagation c.e}
