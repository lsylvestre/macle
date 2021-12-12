(* si [with_memory_access] vaut [true]
   les expressions avec lectures en mémoires 
   deviennent "non transparentes". *)

val transparent : ?with_memory_access:bool -> Ast.TMACLE.exp -> bool
