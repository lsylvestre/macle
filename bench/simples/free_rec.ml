circuit ci(c) =
  let rec f (i) =
    let rec g(j) = 
      if j < i then g(j+1) else c in 
    if i < 10 then g(0) else f(i+1) in
  f(0)

;;;;

print_int (ci 42);;