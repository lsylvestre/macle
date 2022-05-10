open Ast
open TMACLE


let occur (x:ident) (e:exp) : bool =
  let exception Break in
  let f = function
  | Var y
  | App(y,_)
  | Macro(OCamlArrayMap(_,y,_,_))
  | Macro(OCamlArrayReduce(_,y,_,_)) 
  | Macro(OCamlArrayScan(_,y,_,_,_)) ->
     if y = x then raise Break
  | _ -> () in
  try Ast_mapper.check f e; false with Break -> true
