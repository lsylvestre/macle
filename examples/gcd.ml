circuit gcd m n = 
  let rec loop a b =
    if a < b then loop a (b-a) else
    if a > b then loop (a-b) a else a 
  in loop m n

;;;;

print_int (gcd 36 200);;
