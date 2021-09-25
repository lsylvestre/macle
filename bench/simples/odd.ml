circuit s(n) = 
  let rec odd(n) = 
    if n = 0 then false else even(n-1) 
  and even(n) = 
    if n = 0 then true else odd(n-1)
  in
  if odd(42) then 2 else 3

;;;;

