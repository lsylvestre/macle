open Ast.TMACLE
open Occur
open Transparent

(* 
let rec floating e =
  let rec aux e = match e with
  | Var _ | Const _ -> 
      [],e
  | Prim (p,es) ->
      (* [p] do not perform any side effect *)
      let bss,es' = Misc.split_map aux es in
      List.concat bss,Prim (p, es')
  | If(e1,e2,e3) -> 
      let bs1,e1' = aux e1 in
      bs1,If(e1',floating e2,floating e3)
  | Case(e1,hs,e2) -> 
      let bs1,e1' = aux e1 in
      bs1,Case(e1',Misc.map_snd floating hs,floating e2)
  | Let(bs,e) -> 
      let bss,bs' = Misc.split_map (fun (xty,e) -> 
                                      let bs,e' = aux e in 
                                      (bs,(xty,e'))) bs 
      in
      let bs2 = List.concat bss in
      let bs_eff = List.filter (fun (_,e) -> not @@ transparent e) bs' in
      (match bs_eff with
      | _::_ -> bs,floating e
      | [] ->
      let bs0,bs1 = List.partition (fun (_,e) -> 
                       not (List.exists (fun ((x,_),_) -> occur x e) bs2)) bs' in
      let bs3 = bs2@bs0 in
      let bs4,e' = aux e in
      let bs5,bs6 = List.partition (fun (_,e) -> 
                       not (List.exists (fun ((x,_),_) -> occur x e) bs3)) bs4 in
      bs3@bs5,mk_let bs6 e')
  | LetFun(((f,xs),e1),e2) -> 
      let bs2,e2' = aux e2 in
      let bs_eff = List.filter (fun (_,e) -> not @@ transparent e) bs2 in
      (match bs_eff with
      | _::_ -> bs2,LetFun(((f,xs),floating e1),e2')
      | [] ->
      let bs3,bs4 = List.partition (fun (_,e) -> not (occur f e)) bs2 in
      bs3,LetFun(((f,xs),floating e1),mk_let bs4 e2'))
  | LetRec(fs,e) ->
      let bs2,e2 = aux e in
      let bs_eff = List.filter (fun (_,e) -> not @@ transparent e) bs2 in
      let fs' = List.map (fun (xargs,e) -> xargs,floating e) fs in
      (match bs_eff with
      | _::_ -> bs2,LetRec(fs',e2)
      | [] ->
      let bs3,bs4 = List.partition (fun (_,e) -> 
                        not @@ List.exists (fun ((x,_),_) -> occur x e) fs) bs2 in
      let fs' = List.map (fun (xargs,e) -> xargs,floating e) fs in
      bs3,LetRec(fs',mk_let bs4 e2))
  | App(x,es) -> 
      let bss,es' = Misc.split_map aux es in
      let bs1 = List.concat bss in
      bs1,App (x, es')
  | CamlPrim c ->
      [],CamlPrim 
      (match c with 
       | ArrayAccess{arr;idx;ty} ->
           (* let bs1,arr' = aux arr in
           let bs2,idx' = aux idx in
           let bs3,bs4 = List.partition (fun (_,e) -> 
                           not @@ List.exists (fun (x,_) -> occur x e) bs1) bs2 in
           bs1@bs3,mk_let ~ty*) ArrayAccess{arr=floating arr;idx=floating idx;ty}
       | RefAccess (e,ty) ->
           RefAccess (floating e,ty)
       | ArrayLength (e,ty) ->
           ArrayLength (floating e,ty) 
       | ListHd (e,ty) -> 
           ListHd (floating e,ty)
       | ListTl (e,ty) -> 
           ListTl (floating e,ty)
       | RefAssign{r;e;ty} ->
            RefAssign{r=floating r;e=floating e;ty}
       | ArrayAssign{arr;idx;e;ty} ->
           ArrayAssign{arr= floating arr;idx=floating idx;e=floating e;ty}
       | ListFoldLeft(q,ty1,ty2,init,e) ->
           ListFoldLeft(q,ty1,ty2,floating init,floating e) 
       | ArrayFoldLeft(q,ty1,ty2,init,e) ->
           ArrayFoldLeft(q,ty1,ty2,floating init,floating e)
       | ArrayMapBy(n,q,ty,e) ->
           ArrayMapBy(n,q,ty,floating e))
  in 
  let bs,e = aux e in
  mk_let ~ty:(TConst TUnit) bs e (* TODO !! *)
*)
let circuit_let_floating (c : circuit) : circuit = c (* TODO *)
  (* let c = Ast_rename.rename_ast c in *)

  (* {c with e = floating c.e}*)
