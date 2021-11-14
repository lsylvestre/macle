open Ast
open TMACLE

let transparent e =
  let rec aux env (desc,_) =
    match desc with
    | Var _ | Const _ -> true
    | Prim (_,es) ->
        List.for_all (aux env) es
    | If(e1,e2,e3) -> 
        aux env e1 && aux env e2 && aux env e3
    | Case(e1,hs,e2) -> 
        aux env e1 &&
        List.for_all (fun (_,e) -> aux env e) hs &&
        aux env e2
    | Let(bs,e) -> 
        List.for_all (fun (_,e) -> aux env e) bs && aux env e
    | LetFun(((x,_),e1),e2) -> 
        let env' = x::env in 
        aux env' e1 && aux env' e2
    | LetRec(bs,e) ->
        let env' = List.map (fun ((x,_),_) -> x) bs@env in 
        List.for_all (fun (_,e) -> aux env' e) bs &&
        aux env' e
    | App(x,es) -> 
        List.mem x env && List.for_all (aux env) es
    | Match(e,cases) ->
        aux env e &&
        List.for_all (fun (_,_,e) -> aux env e) cases
    | CamlPrim c -> 
        (match c with 
         | ArrayAccess { arr ; idx } ->
             aux env arr || aux env idx
         | (RefAccess e | ArrayLength e | ListHd e | ListTl e) -> 
             aux env e
         | RefAssign _ -> false
         | ArrayAssign _ -> false
         | (ListFoldLeft _ | ArrayFoldLeft _ | ArrayMapBy _) -> 
             assert false (* already expanded *) )
in aux [] e