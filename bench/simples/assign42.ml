circuit assign(r) =
 let rec aux(n) = if n <= 0 then r := 42
  else aux(n-1) in aux(100);;

circuit array_assign(a,i) =
  let rec aux(n) = if n <= 0 then a.(i) <- 42 
  else aux(n-1) in aux(100);;

circuit array_assign_i(a) =
  let rec aux(n) = if n <= 0 then a.(2)<- 42
  else aux(n-1) in aux(100)

;;;;

let pp n = print_int n; print_string " " 

let r = ref 17

pp (!r);;

assign (r);;

pp (!r);;

let a = [|10;11;12;13;14]

array_assign_i(a);;

Array.iter pp a;;

array_assign(a,4);;

Array.iter pp a;;
