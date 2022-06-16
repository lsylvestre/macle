circuit map_inc1 src =
  let inc x = x + 1 in
  array_map 1 inc src src
;;

circuit map_inc2 src =
  let inc x = x + 1 in
  array_map 2 inc src src
;;

circuit map_inc4 src =
  let inc x = x + 1 in
  array_map 4 inc src src
  

;;;;

let n = (32*10) ;;

let a = Array.init n (fun i -> i+1);;

map_inc1 a;

map_inc2 a;

map_inc4 a;

Array.iter (fun x -> print_int x; print_string ";") a;;
