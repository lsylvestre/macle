open Ast
open TMACLE

(* si [with_memory_access] vaut [true]
   les expressions avec lectures en mémoires 
   deviennent "non transparentes". *)

let transparent ?(with_memory_access=false) e =
  let exception Break in
  let f desc env =
    desc,(match desc with
          | LetFun(((x,_),e1),e2) -> 
              x::env
          | LetRec(bs,e) ->
              List.map (fun ((x,_),_) -> x) bs@env
          | App(x,es) -> 
              if List.mem x env then raise Break else env
          | CamlPrim c -> 
              (match c with 
               | ArrayAccess _ | RefAccess _ | ArrayLength _ ->
                   if not with_memory_access then raise Break else env
               | RefAssign _ | ArrayAssign _ ->
                   raise Break
              )
            | _ -> env) in
  try Ast_mapper.iter f [] e; true with Break -> false
