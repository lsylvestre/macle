let gensym_simple =
  let c = ref 0 in
  fun prefix -> incr c; Printf.sprintf "%s_%04d" prefix !c

let gensym =
  let c = ref 0 in
  fun prefix -> incr c;
    let name = match String.split_on_char '#' prefix with
               | [] -> assert false
               | name::_ -> name in
    Printf.sprintf "%s#%x" name !c

