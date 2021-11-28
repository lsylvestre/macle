circuit fact(n) = 
  let rec aux(acc,n) = 
    if n <= 0 then acc else 
    aux(acc*n,n-1) 
  in aux(1,n) ;;

circuit rec gcd(a,b) =
  if a < b then gcd (a,b-a) else
  if a > b then gcd (a-b,a)
           else a

;;;;
(* OCaml program *)

let n = 10;;

for i = 1 to n do
  print_int @@ gcd i (fact i)
done ;;