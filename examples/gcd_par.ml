circuit f x = 
  let rec gcd a b =
    if a < b then gcd a (b-a) else
    if a > b then gcd (a-b) a else a 
  in 
  let x1 = gcd x 120 
  and x2 = gcd x 2 in
  x1 + x2

;;;;

print_int (f 6);;
