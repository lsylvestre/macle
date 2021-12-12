circuit sum l =
  let plus a b = a + b in
  list_fold_left plus 0 l

;;;;;;;

let l = [1;2;3;4;5;6;7;8;9;10] ;;

print_int @@ sum(l);;