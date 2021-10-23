open Ast
open TMACLE
open Gensym

(* renaming of identifiers (variables and functions) *)
   
let r_ident env x = 
  match List.assoc_opt x env with
  | None -> Printf.printf "** %s\n\n" x; assert false
  | Some x' -> x'

let rec r_e env e = match e with
| Var x -> 
   Var (r_ident env x)
| Const _ -> 
    e
| Prim (p,es) ->
    Prim (p, List.map (r_e env) es)
| If(e1,e2,e3,ty) -> 
    If(r_e env e1,r_e env e2,r_e env e3,ty)
| Case(e1,ty,hs,e2,ty2) -> 
    Case(r_e env e1,ty,Misc.map_snd (r_e env) hs,r_e env e2,ty2)
| Let(bs,e,ty) -> 
    let bs' = List.map (fun ((x,ty),e) -> (gensym x,ty),r_e env e) bs in
    let env' = List.map2 (fun ((x,_),_) ((x',_),_) -> x,x') bs bs' @ env in 
    Let(bs',r_e env' e,ty)
| LetFun(((q,xs),e1),e2) ->
    let q' = gensym q in
    let e2' = r_e ((q,q')::env) e2 in 
    let xs' = List.map (fun (x,ty) -> (gensym x,ty)) xs in
    let env' = List.map2 (fun (x,_) (x',_) -> x,x') xs xs' @ env in
    let e1' = r_e env' e1 in
    LetFun(((q',xs'),e1'),e2')
| LetRec(bs,e,ty) -> 
    let qs = List.map (fun ((q,_),_) -> q) bs in
    let qs' = List.map (fun q -> gensym q) qs in
    let env' = List.combine qs qs' @ env in
    let e' = r_e env' e in
    let bs' =
      List.map2
        (fun ((q,xs),e) q' -> 
           let xs' = List.map (fun (x,ty) -> (gensym x,ty)) xs in
           let env'' = List.map2 (fun (x,_) (x',_) -> x,x') xs xs' @ env' in
           let e' = r_e env'' e in
           ((q',xs'),e')) bs qs'
    in
    LetRec(bs',e',ty)
| App(x,es,ty) -> 
    let es' = List.map (r_e env) es in
    App(r_ident env x, es',ty)
| CamlPrim c ->
    CamlPrim 
    (match c with 
     | ArrayAccess{arr;idx;ty} ->
         ArrayAccess{arr=r_e env arr;idx=r_e env idx;ty}
     | RefAccess (e,ty) ->
         RefAccess (r_e env e,ty)
     | ArrayLength (e,ty) ->
         ArrayLength (r_e env e,ty) 
     | ListHd (e,ty) -> 
         ListHd (r_e env e,ty)
     | ListTl (e,ty) -> 
         ListTl (r_e env e,ty)
     | RefAssign{r;e;ty} ->
          RefAssign{r=r_e env r;e= r_e env e;ty}
     | ArrayAssign{arr;idx;e;ty} ->
         ArrayAssign{arr= r_e env arr;idx= r_e env idx;e= r_e env e;ty}
     | ListFoldLeft(q,ty1,ty2,init,e) ->
         ListFoldLeft(r_ident env q,ty1,ty2,r_e env init,r_e env e) 
     | ArrayFoldLeft(q,ty1,ty2,init,e) ->
         ArrayFoldLeft(r_ident env q,ty1,ty2,r_e env init,r_e env e)
     | ArrayMapBy(n,q,ty,e) ->
         ArrayMapBy(n,r_ident env q,ty,r_e env e))

let rename_ast ({xs;e} as p) = 
  let env = List.map (fun (x,_) -> x,x) xs in
  { p with xs; e = r_e env e } 
