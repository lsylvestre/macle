circuit calcul a = 
  let x = let rec f x = f x in f a in
  let y = let rec f x = f x in f a in
  x + y
 

;;;;
