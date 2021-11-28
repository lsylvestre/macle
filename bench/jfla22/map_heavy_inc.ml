circuit map_heavy_inc(a) =
  let heavy_inc(n) =
    let rec add(acc,n) =  
      if n <= 0 then acc else add(acc+1,n-1)
    in add(1,n)
  in
  array_map_by(1,heavy_inc,a)

;;;;
(* programme OCaml *)
let size = 100*64;; 
let n = 100*100;;

let a = Array.init size (fun _ -> Random.int n);;

map_heavy_inc a;;