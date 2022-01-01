open Ast
open TMACLE
open Gensym

(* renaming of identifiers (variables and functions) *)
   
let r_e ?(unbound_variable_err=true) env e = 

  let r_ident env x = 
  match List.assoc_opt x env with
  | None -> if not unbound_variable_err then x else
            (Printf.printf "** %s\n\n" x; assert false)
  | Some x' -> x' in


  let rec mapper ~default env ((desc,ty) as e) = 
  (fun desc -> desc,ty) @@
  match desc with
  | Var x -> 
      Var (r_ident env x)
  | Let(bs,e) -> 
      let bs' = List.map (fun ((x,ty),e) -> (gensym x,ty),mapper ~default env e) bs in
      let env' = List.map2 (fun ((x,_),_) ((x',_),_) -> x,x') bs bs' @ env in 
      Let(bs',mapper ~default env' e)
  | LetFun(((q,xs),e1),e2) ->
      let q' = gensym q in
      let xs' = List.map (fun (x,ty) -> (gensym x,ty)) xs in
      let env' = List.map2 (fun (x,_) (x',_) -> x,x') xs xs' @ env in
      let e1' = mapper ~default env' e1 in
      let e2' = mapper ~default ((q,q')::env) e2 in 
      LetFun(((q',xs'),e1'),e2')
  | LetRec(bs,e) -> 
      let qs = List.map (fun ((q,_),_) -> q) bs in
      let qs' = List.map (fun q -> gensym q) qs in
      let env' = List.combine qs qs' @ env in
      let bs' =
        List.map2
          (fun ((q,xs),e) q' -> 
             let xs' = List.map (fun (x,ty) -> (gensym x,ty)) xs in
             let env'' = List.map2 (fun (x,_) (x',_) -> x,x') xs xs' @ env' in
             let e' = mapper ~default env'' e in
             ((q',xs'),e')) bs qs'
      in
      let e' = mapper ~default env' e in
      LetRec(bs',e')
  | App(x,es) -> 
      let es' = List.map (mapper ~default env) es in
      App(r_ident env x, es')
  | Match(e,cases) ->
     let e' = mapper ~default env e in
     let cases' = List.map (fun (c,xs,e) -> 
                   let ys = List.filter_map fst xs in
                   let ys' = List.map (fun _ -> Gensym.gensym "a") ys in
                   let env' = List.combine ys ys' @ env in 
                   (c,List.map (function (Some x,ty) -> (Some (r_ident env' x),ty) | pty -> pty) xs,
                      mapper ~default env' e)) cases in
    Match(e',cases')
   | FlatArrayOp(Map((xs,e),es)) ->
       let xs' = List.map (fun (x,t) -> gensym "x",t) xs in
       let env' = (List.map2 (fun (x,_) (x',_) -> x,x') xs xs')@env in
       FlatArrayOp(Map((xs',mapper ~default env' e),List.map (mapper ~default env) es))
  | FlatArrayOp(Reduce(((acc,t1),(y,t2),e0),init,e)) ->
       let acc' = gensym "acc" in
       let y' = gensym "y" in
       let e0' = mapper ~default ((acc,acc')::(y,y')::env) e0 in
       FlatArrayOp(Reduce(((acc',t1),(y',t2),e0'),init,e))

  | Macro(OCamlArrayFoldLeft(f,init,e)) ->
      Macro(OCamlArrayFoldLeft(r_ident env f, mapper ~default env init, mapper ~default env e))

  | Macro(OCamlArrayReduceBy(n,f,init,e)) ->
      Macro(OCamlArrayReduceBy(n,r_ident env f, mapper ~default env init, mapper ~default env e))

  | Macro(OCamlArrayIterBy(n,f,e)) ->
      Macro(OCamlArrayIterBy(n,r_ident env f, mapper ~default env e))
 
  | Macro(OCamlArrayMapBy(n,f,e)) ->
      Macro(OCamlArrayMapBy(n,r_ident env f, mapper ~default env e))

  | Macro(Map(f,es)) ->
      Macro(Map(r_ident env f, List.map (mapper ~default env) es))

  | Macro(Reduce(f,e1,e2)) -> 
      Macro(Reduce(r_ident env f, mapper ~default env e1, mapper ~default env e2))

| _ -> 
   fst (default env e)
in
Ast_mapper.map mapper env e


let rename_exp = r_e ~unbound_variable_err:false []

let rename_ast ({xs;e} as p) = 
  let env = List.map (fun (x,_) -> x,x) xs in
  { p with xs; e = r_e env e } 
