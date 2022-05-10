circuit max a b =
  if a > b then a else b
;;

circuit collatz n = 
  let rec next len u = if u <= 1 then len
  else if u mod 2 = 0 then next (len+1) (u/2)
  else next (len+1) (3*u+1) in next 0 n

;;;;

let main() =
  let m = ref 0 in
  for i = 1 to 1000 do
    m := max !m (collatz i)
  done;
  !m
;;

let run = main() ;;