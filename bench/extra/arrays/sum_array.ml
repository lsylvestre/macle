
circuit sum_array(a) =
  let add(x,y) = x + y in
  array_fold_left(add,0,a)

;;;;

let a = [|1;2;3;4;5;6;7;8;9|];;

print_int @@ sum_array a;;
