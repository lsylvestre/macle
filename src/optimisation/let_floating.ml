open Ast.TMACLE
open Occur


let transparent e =
  Transparent.transparent ~with_memory_access:true e

let rec floating e : exp =
  let rec aux ((d,ty) as e) =
  match d with
  | Var _ | Const _ ->
      [],e
  | Unop(p,e) ->
      let bs,e' = aux e in
      bs,(Unop(p,e'),ty)
  | Binop(p,e1,e2) ->
      let bs1,e1' = aux e1 in
      let bs2,e2' = aux e2 in
      bs1@bs2,(Binop(p,e1',e2'),ty)
  | If(e1,e2,e3) ->
      let bs1,e1' = aux e1 in
      bs1,(If(e1',floating e2,floating e3),ty)
  | Let(bs,e) ->
      let bss,bs' = Misc.split_map (fun (xty,e) ->
                                      let bs,e' = aux e in
                                      (bs,(xty,e'))) bs
      in
      let bs2 = List.concat bss in
      let bs_eff = List.filter (fun (_,e) -> not @@ transparent e) bs' in
      (match bs_eff with
      | _::_ -> bs2,(Let(bs',floating e),ty)
      | [] ->
          let bs0,bs1 =
            List.partition (fun (_,e) ->
                not (List.exists (fun ((x,_),_) -> occur x e) bs2)) bs' in
          let bs3 = bs2@bs0 in
          let bs4,e' = aux e in
          let bs5,bs6 = List.partition (fun (_,e) ->
              not (List.exists (fun ((x,_),_) -> occur x e) bs1)) bs4 in
          bs3,mk_let (bs1@bs5) @@ mk_let bs6 e')
  | LetFun(((f,xs),e1),e2) ->
      let bs2,e2' = aux e2 in
      let bs_eff = List.filter (fun (_,e) -> not @@ transparent e) bs2 in
      (match bs_eff with
      | _::_ -> bs2,(LetFun(((f,xs),floating e1),e2'),ty)
      | [] ->
      let bs3,bs4 = List.partition (fun (_,e) -> not (occur f e)) bs2 in
      bs3,(LetFun(((f,xs),floating e1),mk_let bs4 e2'),ty))
  | LetRec(fs,e) ->
      let bs2,e2 = aux e in
      let bs_eff = List.filter (fun (_,e) -> not @@ transparent e) bs2 in
      let fs' = List.map (fun (xargs,e) -> xargs,floating e) fs in
      (match bs_eff with
      | _::_ -> bs2,(LetRec(fs',e2),ty)
      | [] ->
          let bs3,bs4 =
              List.partition (fun (_,e) ->
                not @@ List.exists (fun ((x,_),_) -> occur x e) fs) bs2 in
          let fs' = List.map (fun (xargs,e) -> xargs,floating e) fs in
          bs3,(LetRec(fs',mk_let bs4 e2),ty))
  | App(x,es) ->
      let bss,es' = Misc.split_map aux es in
      let bs1 = List.concat bss in
      bs1,(App (x, es'),ty)
  | Match (e,hs) ->
      let bs2,e2 = aux e in
       bs2,(Match(e2,List.map (fun (c,xs,e) -> (c,xs,floating e)) hs),ty)
  | Raise _ ->
      [],e
  | CamlPrim c ->
      [],(CamlPrim
      (match c with
       | ArrayAccess{arr;idx} ->
           ArrayAccess{arr=floating arr;idx=floating idx}
       | RefAccess e ->
           RefAccess (floating e)
       | ArrayLength e ->
           ArrayLength (floating e)
       | RefAssign{r;e} ->
            RefAssign{r=floating r;e=floating e}
       | ArrayAssign{arr;idx;e} ->
           ArrayAssign{arr= floating arr;idx=floating idx;e=floating e}),ty)
    | FlatArrayOp c ->
        (* todo: faire remonter les liaisons si possible  *)
        [],(FlatArrayOp
            (match c with
             | FlatMake es ->
                 FlatMake (List.map floating es)
             | FlatGet{e;idx} ->
                  FlatGet{e = floating e ; idx = floating idx}
             | ArraySub(e,idx,n) ->
                  ArraySub(floating e,floating idx,n)
             | _ -> assert false
             ),ty)
    | Macro _ ->
       assert false (* already expanded *)
  in
  let bs,e = aux e in
  mk_let bs e



let circuit_let_floating (c : circuit) : circuit =
  let c = Ast_rename.rename_ast c in
  {c with e = floating c.e}
