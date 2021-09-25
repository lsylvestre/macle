circuit composition(k) = 
  let inc(m) = m + 1 in
  let test(b) = if b then 42 else 17 in
  let compose(f,g,x) = f(g(x)) in 
  compose(inc,test,k)
;;

circuit len(a) = 
  array_length a ;;

circuit hd(l) =
  let rec wait(n) = 
    if n <= 0 then list_hd l 
  else wait(n-1) in 
  wait(1000) ;;

circuit tl(l) =
  let rec wait(n) = 
    if n <= 0 then list_tl l
  else wait(n-1) in 
  wait(1000);;

circuit array_get(a,i) = 
  let rec wait(n) = 
    if n <= 0 then a.(i)
  else wait(n-1) in 
  wait(1000);;

circuit array_set(a,i,v) = 
  a.(i) <-v

;;;;

let print_array a = 
  Array.iter (fun n -> print_int n; print_string ",") a;
  print_string "---------\n";;

print_int(composition(true));;
print_string "----\n";;
print_int (len [|1;2;3;4;5;6;7|]) ;;
print_string "----\n";;
print_int (hd [42;43;44]);;
print_string "----\n";;
print_int (List.hd @@ tl [42;43;44]);;
print_string "----\n";;

let a = [|10;11;12;13;14|];;

print_int (array_get(a,2));;
print_string "----\n";;
array_set(a,2,42);;

print_array a ;;
