open Ast
open TMACLE


let occur (x:ident) (e:exp) : bool =
  let exception Break in
  let f = function
  | Var y
  | App(y,_)
  | Macro(Map(y,_))
  | Macro(Reduce(y,_,_))
  | Macro(OCamlArrayMapBy(_,y,_))
  | Macro(OCamlArrayFoldLeft(y,_,_)) ->
     if y = x then raise Break
  | _ -> () in
  try Ast_mapper.check f e; false with Break -> true
