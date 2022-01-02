(** [circuit_let_floating c] transform [c]
    by moving let-bindings in order to produce a faster code *)

val circuit_let_floating :
  Ast.TMACLE.circuit -> Ast.TMACLE.circuit
