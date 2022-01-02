open Ast
open TMACLE

let iter f env e =
  let rec aux env (desc,_) =
    let desc,env = f desc env in
    match desc with
    | Var _ | Const _ -> ()
    | Unop(_,e) ->
        aux env e
    | Binop(_,e1,e2) ->
        aux env e1;
        aux env e2
    | If(e1,e2,e3) ->
        aux env e1;
        aux env e2;
        aux env e3
    | Let(bs,e) ->
        List.iter (fun (_,e) -> aux env e) bs;
        aux env e
    | LetFun((_,e1),e2) ->
        aux env e1;
        aux env e2
    | LetRec(bs,e) ->
        List.iter (fun (_,e) -> aux env e) bs;
        aux env e
    | App(x,es) ->
        List.iter (aux env) es
    | Match(e,cases) ->
        aux env e;
        List.iter (fun (_,_,e) -> aux env e) cases
    | Raise _ ->
        ()
    | CamlPrim c ->
        (match c with
         | ArrayAccess { arr ; idx } ->
             aux env arr;
             aux env idx
         | (RefAccess e | ArrayLength e) ->
             aux env e
         | RefAssign{r;e} ->
             aux env r;
             aux env e
         | ArrayAssign {arr;idx;e} ->
             aux env arr;
             aux env idx;
             aux env e)
    | FlatArrayOp c ->
        (match c with
         | FlatMake es ->
             List.iter (aux env) es
         | FlatGet {e;idx} ->
             aux env e;
             aux env idx
         | ArraySub(e,idx,_) ->
             aux env e;
             aux env idx
         | FlatMap((_,e),es) ->
             aux env e;
             List.iter (aux env) es
         | FlatReduce((_,_,e0),init,e) ->
             aux env e0;
             aux env init;
             aux env e)
    | Macro c ->
        (match c with
         | LazyOr(e1,e2)
         | LazyAnd(e1,e2) ->
             aux env e1;
             aux env e2
         | Map(_,es) ->
             List.iter (aux env) es
         | Reduce(_,init,e) ->
             aux env init;
             aux env e
         | ArrayUpdate{arr;idx;e} ->
             aux env arr;
             aux env idx;
             aux env e
         | OCamlArrayReduceBy(_,_,init,e) ->
             aux env init;
             aux env e
         | OCamlArrayIterBy(_,_,e) ->
             aux env e
         | OCamlArrayFoldLeft (_,init,e) ->
             aux env init;
             aux env e
         | OCamlArrayMapBy (_,_,e) ->
             aux env e)
  in
  aux env e

let check f e =
  iter (fun desc () -> f desc; desc,()) () e



let map f env e =
  let rec aux env e =
    f ~default env e
  and default env (desc,decoration) =
    (fun desc -> desc,decoration) @@
    match desc with
    | Var _ | Const _ -> desc
    | Unop(op,e) ->
        let e' = aux env e in
        Unop(op,e')
    | Binop(op,e1,e2) ->
        let e1' = aux env e1 in
        let e2' = aux env e2 in
        Binop(op,e1',e2')
    | If(e1,e2,e3) ->
        let e1' = aux env e1 in
        let e2' = aux env e2 in
        let e3' = aux env e3 in
        If(e1',e2',e3')
    | Let(bs,e) ->
      let bs' =
        List.map (fun (x,e) -> let e' = aux env e in (x,e')) bs
      in
      let e' = aux env e in
        Let(bs',e')
    | LetFun((x,e1),e2) ->
        let e1' = aux env e1 in
        let e2' = aux env e2 in
        LetFun((x,e1'),e2')
    | LetRec(bs,e) ->
        let bs' =
          List.map (fun (args,e) -> let e' = aux env e in (args,e')) bs
        in
        let e' = aux env e in
        LetRec(bs',e')
    | App(x,es) ->
        let es' = List.map (aux env) es in
        App(x,es')
    | Match(e,cases) ->
        let e' = aux env e in
        let cases' =
          List.map (fun (c,xs,e) -> let e' = aux env e in (c,xs,e')) cases
        in
        Match(e',cases')
    | Raise _ ->
      desc
    | CamlPrim c ->
        CamlPrim
          (match c with
           | ArrayAccess { arr ; idx } ->
               let arr' = aux env arr in
               let idx' = aux env idx in
               ArrayAccess { arr = arr' ; idx = idx' }
           | RefAccess e ->
               let e' = aux env e in
               RefAccess e'
           | ArrayLength e ->
               let e' = aux env e in
               ArrayLength e'
           | RefAssign{r;e} ->
               let r' = aux env r in
               let e' = aux env e in
             RefAssign{r = r' ; e = e'}
           | ArrayAssign {arr;idx;e} ->
               let arr' = aux env arr in
               let idx' = aux env idx in
               let e' = aux env e in
               ArrayAssign{arr = arr' ; idx = idx' ; e = e'})
    | FlatArrayOp c ->
        FlatArrayOp
          (match c with
           | FlatMake es ->
               let es' = List.map (aux env) es in
               FlatMake es'
           | FlatGet {e;idx} ->
               let e' = aux env e in
               let idx' = aux env idx in
               FlatGet{e = e';idx = idx'}
           | ArraySub(e,idx,n) ->
               let e' = aux env e in
               let idx' = aux env idx in
             ArraySub(e',idx',n)
           | FlatMap((xs,e),es) ->
               let e' = aux env e in
               let es' = List.map (aux env) es in
               FlatMap((xs,e'),es')
           | FlatReduce((x,y,e0),init,e) ->
               let e0' = aux env e0 in
               let init' = aux env init in
               let e' = aux env e in
               FlatReduce((x,y,e0'),init',e'))
    | Macro c ->
        Macro
          (match c with
           | LazyOr(e1,e2) ->
               let e1' = aux env e1 in
               let e2' = aux env e2 in
               LazyOr(e1',e2')
           | LazyAnd(e1,e2) ->
               let e1' = aux env e1 in
               let e2' = aux env e2 in
               LazyAnd(e1',e2')
           | Map (f,es) ->
               let es' = List.map (aux env) es in
               Map (f,es')
           | Reduce (f,init,e) ->
               let init' = aux env init in
               let e' = aux env e in
               Reduce(f,init',e')
           | ArrayUpdate{arr;idx;e} ->
               let arr' = aux env arr in
               let idx' = aux env idx in
               let e' = aux env e in
               ArrayUpdate{arr = arr' ; idx = idx' ; e = e'}
           | OCamlArrayReduceBy(n,f,init,e) ->
               let init' = aux env init in
               let e' = aux env e in
               OCamlArrayReduceBy(n,f,init',e')
           | OCamlArrayIterBy(n,f,e) ->
               let e' = aux env e in
               OCamlArrayIterBy(n,f,e')
           | OCamlArrayFoldLeft(f,init,e) ->
               let init' = aux env init in
               let e' = aux env e in
               OCamlArrayFoldLeft(f,init',e')
           | OCamlArrayMapBy(n,f,e) ->
               let e' = aux env e in
               OCamlArrayMapBy(n,f,e'))
  in
  aux env e
