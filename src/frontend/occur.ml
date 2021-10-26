open Ast
open TMACLE

let rec occur x (desc,_) =
  match desc with
  | Var x' ->
      x = x'
  | Const _ ->
      false
  | Prim (_,es) ->
      List.exists (occur x) es
  | If(e1,e2,e3) -> 
      occur x e1 || occur x e2 || occur x e3
  | Case(e1,hs,e2) -> 
      occur x e1 ||
      List.exists (fun (_,e) -> occur x e) hs ||
      occur x e2
  | Let(bs,e) -> 
      List.exists (fun (_,e) -> occur x e) bs ||
      (List.for_all (fun ((x',_),_) -> x <> x') bs && occur x e)
  | LetFun(((q,xs),e1),e2) -> 
      (List.for_all (fun (y,_) -> y <> x) xs && occur x e1) ||
      (x <> q && occur x e2)
  | LetRec(bs,e) -> 
      (List.for_all (fun ((x',xs),_) ->
                       x <> x' && List.for_all (fun (y,_) -> x <> y) xs) bs) && 
      (List.exists (fun ((_,_),e) -> occur x e) bs || occur x e)
  | App(x',es) -> 
      x' = x || List.exists (occur x) es
  | CamlPrim c -> 
      (match c with 
       | ArrayAccess{arr;idx} ->
           occur x arr || occur x idx
       | (RefAccess e | ArrayLength e | ListHd e | ListTl e) -> 
           occur x e
       | RefAssign{r;e} ->
           occur x r || occur x e
       | ArrayAssign{arr;idx;e} ->
           occur x arr || occur x idx || occur x e
       | (ListFoldLeft _ | ArrayFoldLeft _ | ArrayMapBy _) -> 
           assert false (* already expanded *) )
