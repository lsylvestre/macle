circuit odd_even n = 
  let rec odd n = 
    n > 0 && even (n-1) 
  and even n = 
    n <= 0 || odd (n-1) 
in
  if odd 42 then 2 else 3

;;;;

