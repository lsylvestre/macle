circuit sum_square(m) = 
  let sigma(f,k) =
    let rec s(n,acc) =
      if n <= 0 then acc else s(n-1,acc+f(n)) in
    s(k,0) 
  in
  let square(x) = x * x in
  sigma(square,m)

;;;

print_int (sum_square(10))