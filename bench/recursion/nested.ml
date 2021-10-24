circuit ci(n) = 
  let rec f1(x) =
    let rec f2(m) = 
      if m = x then f1(m+x) else f2(x) in 
    if false then f2(x) else 17
  in f1(n) 

;;;