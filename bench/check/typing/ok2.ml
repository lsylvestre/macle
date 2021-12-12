circuit f y = 
  let compose f g x = f (g x) in
  let f x = x + 1 in
  compose f f y

;;;;