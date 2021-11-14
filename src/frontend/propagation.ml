open Ast.TMACLE

(** constant/copy propagation 
    e.g. [let x = y and z = 2 in x + z + 1] ~> [y + 2 + 1] *)

(** Assume that each identifier binded by a Let is unique:
    a renaming of identifiers must be perform on the source code beforehand. *)

let propagation (e:exp) =
  let rec aux env (desc,ty) = 
  (fun desc' -> (desc',ty)) @@
  match desc with
  | Var x -> (match List.assoc_opt x env with None -> desc | Some e' -> e')
  | Const _ -> 
      desc
  | Prim (p,es) ->
      Prim (p, List.map (aux env) es)
  | If(e1,e2,e3) -> 
      If(aux env e1,aux env e2,aux env e3)
  | Case(e1,hs,e2) -> 
      Case(aux env e1,Misc.map_snd (aux env) hs,aux env e2)
  | Let(bs,e) ->
      let bs' = Misc.map_snd (aux env) bs in
      let env_ext,bs = List.partition_map 
        (function 
         | ((x,_),(((Var _ | Const _) as dc),_)) -> Left (x,dc) 
         | b -> Right b) bs' 
      in
      fst (mk_let bs (aux (env_ext@env) e))
  | LetFun((qxs,e1),e2) -> 
      LetFun((qxs,aux env e1),aux env e2)
  | LetRec(bs,e) -> 
      LetRec(Misc.map_snd (aux env) bs,aux env e)
  | App(x,es) -> 
      let es' = List.map (aux env) es in
      (match List.assoc_opt x env with 
       | None -> App(x,es')
       | Some (Var q) -> App(q,List.map (aux env) es)
       | _ -> assert false)
  | Match(e,cases) ->
      let cases' = List.map (fun (c,xs,e) -> c,xs,aux env e) cases in
      Match(aux env e,cases')
  | CamlPrim c ->
      CamlPrim 
      (match c with 
       | ArrayAccess { arr ; idx } ->
           ArrayAccess { arr = aux env arr ; idx = aux env idx }
       | RefAccess e ->
           RefAccess (aux env e)
       | ArrayLength e ->
           ArrayLength (aux env e) 
       | ListHd e -> 
           ListHd (aux env e)
       | ListTl e -> 
           ListTl (aux env e)
       | RefAssign{r;e} ->
            RefAssign { r = aux env r ; e = aux env e }
       | ArrayAssign { arr ; idx ; e } ->
           ArrayAssign { arr = aux env arr ; idx = aux env idx ; e = aux env e }
       | ListFoldLeft(q,init,e) ->
           ListFoldLeft(q,aux env init,aux env e) 
       | ArrayFoldLeft(q,init,e) ->
           ArrayFoldLeft(q,aux env init,aux env e)
       | ArrayMapBy(n,q,e) ->
           ArrayMapBy(n,q,aux env e))
in aux [] e


let constant_copy_propagation (c : circuit) : circuit = 
  {c with e = propagation c.e}
