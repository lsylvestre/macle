(** atom propagation,

    e.g. [let x = y + 1 in x + x] ~> [(y + 1) + (y + 1)] *)

(** Assume that each identifier binded (by Let/LetFun/LetRec etc.) is unique:
    a renaming of identifiers must be perform on the source code beforehand. *)

val constant_copy_propagation :
  Ast.TMACLE.circuit -> Ast.TMACLE.circuit
