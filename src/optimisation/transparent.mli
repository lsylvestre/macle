(** [transparent e] is [true] iff the expression [e] is without side-effect.

    if [with_memory_access] is enable,
    memory read (e.g. array_length) are seen as "non transparent". *)

val transparent : ?with_memory_access:bool -> Ast.TMACLE.exp -> bool
