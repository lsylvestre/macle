(* expand :
   - [or e1 e2] into [if e1 then true else e2]
   - [and e1 e2] into [if e1 then e2 else false]
   - parallel skeletons
*)

val expand_circuit :
  Ast.TMACLE.circuit -> Ast.TMACLE.circuit
