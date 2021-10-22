circuit map1(a) =
  let inc(n) =
    let rec add(acc,n) =  
      if n <= 0 then acc else add(acc+1,n-1)
    in add(1,n)
  in
  array_map_by(1,inc,a) ;;

circuit map2(a) =
  let inc(n) =
    let rec add(acc,n) =  
      if n <= 0 then acc else add(acc+1,n-1)
    in add(1,n)
  in
  array_map_by(2,inc,a) ;;

circuit map4(a) =
  let inc(n) =
    let rec add(acc,n) =  
      if n <= 0 then acc else add(acc+1,n-1)
    in add(1,n)
  in
  array_map_by(4,inc,a) ;;


circuit map16(a) =
  let inc(n) =
    let rec add(acc,n) =  
      if n <= 0 then acc else add(acc+1,n-1)
    in add(1,n)
  in
  array_map_by(16,inc,a) ;;

circuit map64(a) =
  let inc(n) =
    let rec add(acc,n) =  
      if n <= 0 then acc else add(acc+1,n-1)
    in add(1,n)
  in
  array_map_by(64,inc,a)

;;;;

let print_int = Serial.write_int ;;
let print_string = Serial.write_string ;;

let print_array a = 
  Array.iter (fun n -> print_int n; print_string ",") a;
  print_string "---------\n"

let mk_array () = Array.init (1*64) (fun x -> x mod 40 + 1);;

let a = mk_array ();;
map1 (a);;
print_array a;;

let a = mk_array ();;
map2 (a);;
print_array a;;

let a = mk_array ();;
map4 (a);;
print_array a;;

let a = mk_array ();;
map16 (a);;
print_array a;;

let a = mk_array ();;
map16 (a);;
print_array a;;
print_string "======\n"