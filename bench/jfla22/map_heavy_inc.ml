circuit map_heavy_inc1 a =
  let heavy_inc n =
    let rec add acc n =  
      if n <= 0 then acc else add (acc+1) (n-1)
    in add 1 n
  in
  array_map_by 1 heavy_inc a;;

circuit map_heavy_inc2 a =
  let heavy_inc n =
    let rec add acc n =  
      if n <= 0 then acc else add (acc+1) (n-1)
    in add 1 n
  in
  array_map_by 2 heavy_inc a;;

circuit map_heavy_inc4 a =
  let heavy_inc n =
    let rec add acc n =  
      if n <= 0 then acc else add (acc+1) (n-1)
    in add 1 n
  in
  array_map_by 4 heavy_inc a;;

circuit map_heavy_inc8 a =
  let heavy_inc n =
    let rec add acc n =  
      if n <= 0 then acc else add (acc+1) (n-1)
    in add 1 n
  in
  array_map_by 8 heavy_inc a;;

circuit map_heavy_inc16 a =
  let heavy_inc n =
    let rec add acc n =  
      if n <= 0 then acc else add (acc+1) (n-1)
    in add 1 n
  in
  array_map_by 16 heavy_inc a

;;;;
(* programme OCaml *)
let size = 1*64;; 
let n = 1000*1000;;

let a = Array.init size (fun _ -> Random.int n);;

map_heavy_inc1 a;;