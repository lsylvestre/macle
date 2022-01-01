circuit s n = 
  let inc a = a+1 in
  let apply f x = f x in
  let apply2 f x y = f x y in 
  apply2 apply inc n

;;;;

