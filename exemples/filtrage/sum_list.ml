circuit sum l = 
  let rec aux a l =
    match l with
    | [] -> a
    | x::l -> aux (x + a) l in
  aux 0 l

;;;;

print_int @@ sum [1;2;3;4;5;6;7;8;9;10] ;;