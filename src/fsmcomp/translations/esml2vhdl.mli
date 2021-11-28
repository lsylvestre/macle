val allow_heap_access : bool ref
val allow_heap_assign : bool ref
val allow_trap : bool ref

val caml_heap_base : string

val c_ty : Format.formatter -> Ktypes.ty -> unit

val compile_esml_circuit : 
  ?reset:string -> ?clock:string -> 
  Format.formatter -> Kast.ESML.circuit -> unit
 