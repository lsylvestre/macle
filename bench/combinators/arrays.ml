circuit get(a,i) =
  a.(i) ;;

circuit len(a) =
  array_length(a) 

;;

circuit set(a,i,x) =
  a.(i) <- x

;;;

let a = [|1;2;42;4;5|];;

print_int (get a 3); print_string ";"

set a 1 17;;

Array.iter (fun x -> print_int x; print_string ";") a