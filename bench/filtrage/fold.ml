circuit sum l =
  let fold f init l = 
    let rec aux acc l =
      match l with
        [] -> acc
      | x::xs -> let v = f x acc in aux v xs in
    aux init l in
  let add x y = x + y in
  fold add 0 l

;;;;

print_int @@ sum [1;2;3;4;5;6;7;8;9;10] ;;