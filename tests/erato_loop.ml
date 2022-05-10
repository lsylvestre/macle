circuit sieve4 src =
  let rec gcd a b =
    if a > b then gcd (a-b) b else 
    if a < b then gcd a (b-a)
             else a
  in
  let n = array_length src in
  for i = 0 to n-1 do
    let y = src.(i) in
    if y <= 1 then () else
    let f x = 
      if x == 0 then 0 else 
      if x == y then x else 
      if gcd x y == 1 then x else 0
    in
    array_map 4 f src src
  done

;;;;

let n = (32*10) ;;

let a = Array.init n (fun i -> i+1);;

sieve4 a;;

Array.iter (fun x -> print_int x; print_string ";") a;;
