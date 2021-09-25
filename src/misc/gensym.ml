let gensym =
  let c = ref 0 in
  fun prefix -> incr c; Printf.sprintf "%s_%04d" prefix !c