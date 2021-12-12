circuit compose k = 
  let inc m = m + 1 in
  let test b = if b then 42 else 17 in
  let compose f g x = f (g x) in 
  compose inc test k
;;;;

compose true;;