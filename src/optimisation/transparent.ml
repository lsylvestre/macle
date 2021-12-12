open Ast
open TMACLE

(* si [with_memory_access] vaut [true]
   les expressions avec lectures en mémoires 
   deviennent "non transparentes". *)

let transparent ?(with_memory_access=false) e =
  let rec aux env (desc,_) =
    match desc with
    | Var _ | Const _ -> true
    | Unop(_,e) -> 
        aux env e
    | Binop(_,e1,e2) -> 
        aux env e1 && aux env e2
    | If(e1,e2,e3) -> 
        aux env e1 && aux env e2 && aux env e3
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
    | Raise _ -> 
        false
    | CamlPrim c -> 
        (match c with 
         | ArrayAccess { arr ; idx } ->
             not with_memory_access && (aux env arr || aux env idx)
         | (RefAccess e | ArrayLength e) -> 
             not with_memory_access && aux env e
         | RefAssign _ -> false
         | ArrayAssign _ -> false
         | (ArrayFoldLeft _ | ArrayMapBy _) -> 
             assert false (* already expanded *) )
in aux [] e