circuit h x = 
  let f a = a + 1 in
  let y = map f #[|1;2;3|] in
  y[0]

;;;;;
