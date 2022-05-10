open Ast
open TMACLE

let iter f env e =
  let rec aux env e =
    let desc,env = f e env in
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
         | (RefAccess e | ArrayLength e | Ref e) ->
             aux env e
         | RefAssign{r;e} ->
             aux env r;
             aux env e
         | ArrayAssign {arr;idx;e} ->
             aux env arr;
             aux env idx;
             aux env e
         | ArrayMake{size;e} ->
             aux env size;
             aux env e)
    | PacketPrim c ->
        (match c with
         | PkMake es ->
             List.iter (aux env) es
         | PkGet (e,idx) ->
             aux env e;
             aux env idx
         | PkSet(x,idx,e) ->
             aux env idx;
             aux env e
         | ToPacket(e,idx,_) ->
             aux env e;
             aux env idx
         | OfPacket(e1,e2,idx,_) ->
             aux env e1;
             aux env e2;
             aux env idx
         | PkMap((_,e),es) ->
             aux env e;
             List.iter (aux env) es
         | PkReduce((_,_,e0),init,e) | PkScan((_,_,e0),init,e) ->
             aux env e0;
             aux env init;
             aux env e)
    | Macro c ->
        (match c with
         | LazyOr(e1,e2)
         | LazyAnd(e1,e2) ->
             aux env e1;
             aux env e2
         | OCamlArrayReduce (_,_,init,e) ->
             aux env init;
             aux env e
         | OCamlArrayMap(_,_,e1,e2) ->
             aux env e1;
             aux env e2
         | OCamlArrayScan(n,f,init,e,dst) ->
             aux env init;
             aux env e;
             aux env dst)
    | StackPrim c ->
       (match c with
        | Push(_,e) ->
             aux env e
        | Push_arg(e1,e2) ->
             aux env e1;
             aux env e2
        | LetPop(_,e) ->
             aux env e
        | Save (_,e) ->
             aux env e
        | Restore ->
             ())
  in
  aux env e

let check f e =
  iter (fun (desc,_) () -> f desc; desc,()) () e



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
           | Ref e ->
               let e' = aux env e in
               Ref e'
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
               ArrayAssign{arr = arr' ; idx = idx' ; e = e'}
               | ArrayMake { size ; e } ->
               let size' = aux env size in
               let e' = aux env e in
               ArrayMake { size = size' ; e = e' })
    | PacketPrim c ->
        PacketPrim
          (match c with
           | PkMake es ->
               let es' = List.map (aux env) es in
               PkMake es'
           | PkGet(e,idx) ->
               let e' = aux env e in
               let idx' = aux env idx in
               PkGet(e', idx')
           | PkSet(x,idx,e) ->
               let idx' = aux env idx in
               let e' = aux env e in
               PkSet(x,idx',e')
           | ToPacket(e,idx,n) ->
               let e' = aux env e in
               let idx' = aux env idx in
               ToPacket(e',idx',n)
           | OfPacket(e1,e2,idx,n) ->
               let e1' = aux env e1 in
               let e2' = aux env e2 in
               let idx' = aux env idx in
               OfPacket(e1',e2',idx',n)
           | PkMap((xs,e),es) ->
               let e' = aux env e in
               let es' = List.map (aux env) es in
               PkMap((xs,e'),es')
           | PkReduce((x,y,e0),init,e) ->
               let e0' = aux env e0 in
               let init' = aux env init in
               let e' = aux env e in
               PkReduce((x,y,e0'),init',e')
           | PkScan((x,y,e0),init,e) ->
               let e0' = aux env e0 in
               let init' = aux env init in
               let e' = aux env e in
               PkScan((x,y,e0'),init',e'))
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
           | OCamlArrayReduce(n,f,init,e) ->
               let init' = aux env init in
               let e' = aux env e in
               OCamlArrayReduce(n,f,init',e')
           | OCamlArrayMap(n,f,src,dst) ->
               let src' = aux env src in
               let dst' = aux env dst in
               OCamlArrayMap(n,f,src',dst')
           | OCamlArrayScan(n,f,init,e,dst) ->
               let init' = aux env init in
               let e' = aux env e in
               let dst' = aux env dst in
               OCamlArrayScan(n,f,init',e',dst'))
    | StackPrim c ->
       StackPrim
         (match c with
          | Push(xs,e) ->
               let e' = aux env e in
               Push(xs,e')
          | Push_arg(e1,e2) ->
               let e1' = aux env e1 in
               let e2' = aux env e2 in
               Push_arg(e1',e2')
          | LetPop(xs,e) ->
               let e' = aux env e in
               LetPop(xs,e')
          | Save (q,e) ->
               let e' = aux env e in
               Save (q,e')
          | Restore ->
               c)
  in
  aux env e
