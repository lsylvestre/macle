circuit fact n = 
  let rec aux acc n = 
    if n <= 0 then acc else aux (acc*n) (n-1) in
    aux 1 n  ;;

circuit map_inc a =
  let inc n =
    let rec add acc n =  
      if n <= 0 then acc else add (acc+1) (n-1)
    in add 1 n
  in
  array_map_by 1 inc a

;;;

let print_array a = 
  Array.iter (fun n -> print_int n; print_string ",") a;
  print_string "---------\n";;

print_int (fact(6));;

print_string "---------\n";;

let a = [|1;2;3;4;5;6;7;8|];;

print_array a;;

map_inc(a);;

print_array a;;

map_inc(a);;

print_array a;;