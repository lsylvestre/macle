(* open Ast
open TMACLE
open Types

let substitution env (e:exp) =
  let rec aux (desc,ty) =
    (fun desc -> desc,ty) @@
    match desc with
    | Var x ->
      (match List.assoc_opt x env with
       | None -> desc
       | Some y -> Var y)
    | Const _ ->
        desc
    | Unop(p,e) ->
        Unop(p,aux e)
    | Binop(p,e1,e2) ->
        Binop(p,aux e1,aux e2)
    | If(e1,e2,e3) ->
        If(aux e1,aux e2,aux e3)
    | Let(bs,e) ->
        Let(Misc.map_snd aux bs,aux e)
    | LetFun((qxs,e1),e2) ->
        LetFun((qxs,aux e1),aux e2)
    | LetRec(bs,e) ->
        LetRec(Misc.map_snd aux bs,aux e)
    | App(x,es) ->
        let es' = List.map aux es in
        (match List.assoc_opt x env with
         | None -> App(x,es')
         | Some y -> App(y,List.map aux es))
    | Match(e,cases) ->
        let e' = aux e in
        let cases' =
          List.map (fun (c,xs,e) ->
              assert (List.for_all (function
                  | Some x,_ -> not (List.mem_assoc x env)
                  | None,_ -> true) xs);
              (c,xs,aux e)) cases in
        Match(e',cases')
    | Raise _ ->
        desc
    | CamlPrim c ->
        CamlPrim
          (match c with
           | ArrayAccess{arr;idx} ->
               ArrayAccess{ arr = aux arr ; idx = aux idx }
           | RefAccess e ->
               RefAccess (aux e)
           | Ref e -> 
               Ref (aux e)
           | ArrayLength e ->
               ArrayLength (aux e)
           | RefAssign{r;e} ->
               RefAssign{ r = aux r ; e = aux e }
           | ArrayAssign{arr;idx;e} ->
               ArrayAssign{ arr = aux arr ; idx = aux idx ; e = aux e }
           | ArrayMake{size;e} -> 
               let size = aux size in
               let e = aux e in
               ArrayMake{size;e})
    | FlatArrayOp c ->
      FlatArrayOp
        (match c with
         | FlatMake es ->
             FlatMake (List.map aux es)
         | FlatGet{e;idx} ->
             FlatGet {e = aux e ; idx = aux idx}
         | FlatArraySet(x,idx,e) ->
             FlatArraySet(x,aux idx,aux e)
         | ArraySub(e,idx,n) ->
             ArraySub(aux e,aux idx,n)
         | ArrayBlit(e1,e2,idx,n) ->
             ArrayBlit(aux e1,aux e2,aux idx,n)
         | FlatMap((xs,e),es) ->
             FlatMap((xs,aux e),List.map aux es)
         | FlatReduce((x,y,e0),init,e) ->
             FlatReduce((x,y,aux e0),aux init,aux e)
         | FlatScan((x,y,e0),init,e) ->
             FlatScan((x,y,aux e0),aux init,aux e))
    | Macro _ ->
        assert false
    | StackPrim _ ->
      assert false (* not yet introduced *)
in aux e

let rec inline rec_call env (e:exp) : exp =  
  let (desc,ty) = e in
  match desc with
  | Var _ | Const _ | Unop _ | Binop _ -> desc,ty
  | If(a,e1,e2) ->
      If(a,inline rec_call env e1,inline rec_call env e2),ty
  | Let([(x,_),((Var f),_)],e) -> inline rec_call env (substitution [x,f] e)
  | Let(bs,e) ->
      mk_let (List.map (fun (x,e) -> (x,inline rec_call env e)) bs) 
             (inline rec_call env e)
  | LetRec(ds,e) ->
      let ds' = List.map (fun (fargs,e) -> (fargs,(* inline rec_call env*) e)) ds in
      let fs = List.map (fun ((f,_),_) -> f) ds in
      let env' = (List.map (fun f -> f,ds') fs)@env in
      inline rec_call env' e
  | App(x,_) ->
      if List.mem x rec_call then e else
        let ds = List.assoc x env in
        let fs = List.map (fun ((f,_),_) -> f) ds in
        let ds' = List.map (fun (fargs,e) -> (fargs,inline (fs@rec_call) env e)) ds in
        mk_letrec ds' (inline (fs@rec_call) env @@ e)
  | Match(e,cases) ->
      let inline_case (c,xs,e) =
        (c,xs,inline rec_call env e)
      in
      let e' = inline rec_call env e in
      let cases' = List.map inline_case cases in
      Match(e',cases'),ty
  | Raise _ ->
      desc,ty
  | CamlPrim r ->
      let c =
        match r with
        | RefAccess e ->
            RefAccess(inline rec_call env e)
        | RefAssign{r;e} ->
            let r = inline rec_call env r in
            let e = inline rec_call env e in
            RefAssign{ r ; e }
        | Ref e ->
            Ref (inline rec_call env e)
        | ArrayAccess{arr;idx} ->
            let arr = inline rec_call env arr in
            let idx = inline rec_call env idx in
            ArrayAccess{ arr ; idx }
        | ArrayAssign{arr;idx;e} ->
            let arr = inline rec_call env arr in
            let idx = inline rec_call env idx in
            let e = inline rec_call env e in
            ArrayAssign{ arr ; idx ; e }
        | ArrayMake{size;e} ->
            let size = inline rec_call env size in
            let e = inline rec_call env e in
            ArrayMake{size;e}
        | ArrayLength e ->
            let e' = inline rec_call env e in
            ArrayLength(e')
      in
      (CamlPrim c),ty
  | FlatArrayOp c ->
      FlatArrayOp
        (match c with
         | FlatMake es ->
             FlatMake (List.map (inline rec_call env) es)
         | FlatGet{e;idx} ->
             let e = inline rec_call env e in
             let idx = inline rec_call env idx in
             FlatGet{e ; idx}
         | FlatArraySet(x,e,idx) ->
             let idx = inline rec_call env idx in
             let e = inline rec_call env e in
             FlatArraySet(x,e,idx)
         | ArraySub(e,idx,n) ->
             let e' = inline rec_call env e in
             let idx' = inline rec_call env idx in
             ArraySub(e',idx',n)
         | ArrayBlit(e1,e2,idx,n) ->
             let e1' = inline rec_call env e1 in
             let e2' = inline rec_call env e2 in
             let idx' = inline rec_call env idx in
             ArrayBlit(e1',e2',idx',n)
         | FlatMap((xs,e),es) ->
             let e' = inline rec_call env e in
             let es' = List.map (inline rec_call env) es in
             FlatMap((xs,e'),es')
         | FlatReduce((x,y,e0),init,e) ->
             let e0' = inline rec_call env e0 in
             let init' = inline rec_call env init in
             let e' = inline rec_call env e in
             FlatReduce((x,y,e0'),init',e')
         | FlatScan((x,y,e0),init,e) ->
             let e0' = inline rec_call env e0 in
             let init' = inline rec_call env init in
             let e' = inline rec_call env e in
             FlatScan((x,y,e0'),init',e')),ty
  | Macro _ ->
      assert false (* already expanded *)
  | StackPrim _ ->
      assert false (* not yet introduced *)

let inline_circuit (c : TMACLE.circuit) : TMACLE.circuit =
  {c with e = inline [] [] c.e}
*)