val allow_heap_access : bool ref
val allow_heap_assign : bool ref
val allow_trap : bool ref

val caml_heap_base : string


val array_defs : (string * (string * int)) list ref

val c_ty : Format.formatter -> Esml.Typ.t -> unit

val compile_esml_circuit : 
  ?reset:string -> ?clock:string -> 
  Format.formatter -> Esml.circuit -> unit
 