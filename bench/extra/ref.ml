circuit inc(r) = 
  r := !r + 1

;;;;

let r = ref 42;;

inc r;;

print_int !r;;

