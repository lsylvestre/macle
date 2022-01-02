(** [rewrite_map_circuit c] transform [c]
    by applying rewriting rules to simplify map/reduce compositions *)

val rewrite_map_circuit : Ast.TMACLE.circuit -> Ast.TMACLE.circuit
