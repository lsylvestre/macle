circuit h x = 
  let f x y z = x + y + z in
  let u = map f #[|1;2;3|] #[|4;5;6|] #[|7;8;9|] in
  let add x y = x + y in
  reduce add 0 u

;;;;;
