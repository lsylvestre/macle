circuit inc(x) =
 x := !x + 1
;;;;

let r = ref 42;;

inc r;;

print_int !r;;

