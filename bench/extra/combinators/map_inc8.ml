circuit map8(a) =
  let inc(n) =
    let rec add(acc,n) =  
      if n <= 0 then acc else add(acc+1,n-1)
    in add(1,n)
  in
  array_map_by(8,inc,a) 
;;;;

let print_array a = 
  Array.iter (fun n -> print_int n; print_string ",") a;
  print_string "---------\n" ;;


let mk_array n = 
  Array.init n (fun x -> x);;

let a = mk_array 21 ;;

map8 (a);;
print_array a;;

print_string "======\n"