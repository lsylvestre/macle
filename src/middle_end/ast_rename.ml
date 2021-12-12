open Ast
open TMACLE
open Gensym

(* renaming of identifiers (variables and functions) *)
   
let r_ident env x = 
  match List.assoc_opt x env with
  | None -> Printf.printf "** %s\n\n" x; assert false
  | Some x' -> x'

let rec r_e env (e,ty) = 
  (fun e -> e,ty) @@
  match e with
  | Var x -> 
      Var (r_ident env x)
  | Const _ -> 
      e
  | Unop(p,e) ->
      Unop(p,r_e env e) 
  | Binop(p,e1,e2) ->
      Binop(p,r_e env e1,r_e env e2) 
  | If(e1,e2,e3) -> 
      If(r_e env e1,r_e env e2,r_e env e3)
  | Let(bs,e) -> 
      let bs' = List.map (fun ((x,ty),e) -> (gensym x,ty),r_e env e) bs in
      let env' = List.map2 (fun ((x,_),_) ((x',_),_) -> x,x') bs bs' @ env in 
      Let(bs',r_e env' e)
  | LetFun(((q,xs),e1),e2) ->
      let q' = gensym q in
      let xs' = List.map (fun (x,ty) -> (gensym x,ty)) xs in
      let env' = List.map2 (fun (x,_) (x',_) -> x,x') xs xs' @ env in
      let e1' = r_e env' e1 in
      let e2' = r_e ((q,q')::env) e2 in 
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
             let e' = r_e env'' e in
             ((q',xs'),e')) bs qs'
      in
      let e' = r_e env' e in
      LetRec(bs',e')
  | App(x,es) -> 
      let es' = List.map (r_e env) es in
      App(r_ident env x, es')
  | Match(e,cases) ->
     let e' = r_e env e in 
     let cases' = List.map (fun (c,xs,e) -> 
                   let ys = List.filter_map fst xs in
                   let ys' = List.map (fun _ -> Gensym.gensym "a") ys in
                   let env' = List.combine ys ys' @ env in 
                   (c,List.map (function (Some x,ty) -> (Some (r_ident env' x),ty) | pty -> pty) xs,
                      r_e env' e)) cases in
    Match(e',cases')
  | Raise _ as d -> 
      d
  | CamlPrim c ->
      CamlPrim 
      (match c with 
       | ArrayAccess{arr;idx} ->
           ArrayAccess{ arr = r_e env arr ; idx = r_e env idx }
       | RefAccess e ->
           RefAccess (r_e env e)
       | ArrayLength e ->
           ArrayLength (r_e env e)
       | RefAssign{r;e} ->
            RefAssign{ r = r_e env r;e = r_e env e }
       | ArrayAssign{arr;idx;e} ->
           ArrayAssign{ arr = r_e env arr ; idx = r_e env idx ; e = r_e env e }
       | ArrayFoldLeft(q,init,e) ->
           ArrayFoldLeft(r_ident env q,r_e env init,r_e env e)
       | ArrayMapBy(n,q,e) ->
           ArrayMapBy(n,r_ident env q,r_e env e))

let rename_ast ({xs;e} as p) = 
  let env = List.map (fun (x,_) -> x,x) xs in
  { p with xs; e = r_e env e } 
