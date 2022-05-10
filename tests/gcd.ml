circuit gcd a b = 
  let rec loop a b =
    if a < b then loop a (b-a) else
    if a > b then loop (a-b) a else a 
  in loop a b

;;;;

let n = 10_000_000;;

print_int (gcd 1 n);;
