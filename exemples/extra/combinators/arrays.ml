circuit get(a,i) =
  a.(i) ;;

circuit len(a) =
  array_length(a) 

;;

circuit set(a,i,x) =
  a.(i) <- x

;;
circuit hd(l) =
  list_hd(l) 

;;
circuit tl(l) =
  list_tl(l) 

;;
circuit bang(r) =
  !r 

;;
circuit assign(r,x) =
  r := x 


;;;

let r = ref 42;;

print_int (bang r);;

assign r 17;;

print_int (!r);;

let a = [|1;2;42;4;5|];;

print_int (hd [10;11;12]);; print_string ";";;
print_int (List.hd (tl [10;11;12]));; print_string ";";;

print_int (get a 3);; print_string ";";;
print_int (len a);; print_string ";";;

set a 1 17;;

Array.iter (fun x -> print_int x; print_string ";") a