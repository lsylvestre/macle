circuit sum a =
  let pk = of_array 10 a in 
  let add x y = x + y in
  reduce add 0 pk

;;;;;


print_int @@ sum (Array.init 10 (fun x -> x + 1));;
