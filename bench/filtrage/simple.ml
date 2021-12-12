circuit f l = 
  let rec fact a n =
    if n = 0 then a else fact (a*n) (n-1) in 
  match l with
  | [] -> fact 1 6
  | _::_ -> fact 1 7

;;;;

print_int @@ f [1;2;3;4;5;6;7;8;9;10] ;;