circuit g x = 
  let a = #[|1;2;3|] in
  let a' = a with x := 42 in
  a'[x]

;;;;;
