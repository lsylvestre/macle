circuit sum a = 
  let u = a + 1 in
  let add x y = x + y in
  let inc x = x + 1 in
  reduce add 0 (map inc #[|0;1;2;3;4;5;6;7;8;9|])

;;;;;


print_int @@ sum ();;