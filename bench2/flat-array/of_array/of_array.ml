circuit sum a =
  let pk = of_array 5 a in 
  let add x y = x + y in
  reduce add 0 pk

;;;;;


print_int @@ sum (Array.init 5 (fun x -> x));;
