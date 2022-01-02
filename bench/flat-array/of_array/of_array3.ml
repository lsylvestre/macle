circuit sum a =
  let pk = of_array 30 a in 
  let add x y = x + y in
  let fib n = 
    let rec f a b n = 
      if n <= 0 then a else f b (a+b) (n-1) in 
      f 1 1 n 
  in
  reduce add 0 (map fib pk)

;;;;;


print_int @@ sum (Array.init 30 (fun x -> x + 1));;
