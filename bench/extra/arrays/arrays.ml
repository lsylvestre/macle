circuit last(x) =
 x.(array_length x - 1);;


circuit f(x) =
 x.(x.(2))

;;;;;

let a = [|1;2;3;4;5;6;7;8;9|];;

print_int @@ f(a);;
