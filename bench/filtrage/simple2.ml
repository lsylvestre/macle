circuit f (l) = 
  let rec fact (a,n) =
    if n = 0 then a else fact(a*n,n-1) in 
  match l with
  | [] -> fact(1,6)
  | x::_ -> fact(1,x)

;;;;

print_int @@ f [7;2;3;4;5;6;7;8;9;10] ;;