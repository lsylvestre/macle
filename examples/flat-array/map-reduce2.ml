circuit sum a = 
  let add x y = x + y in
  let inc x = x + 1 in
  let v = map inc #[|0;1|] in
  let w = map inc v in
  reduce add 0 (map add w w)

;;;;;


print_int @@ sum ();;