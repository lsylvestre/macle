circuit calcul r = 
  let x = r := 1 in
  if let y = let rec f x = 
                let k = r := 3 in 
                if !r = 3 then true else f x 
             in 
             let h = r := 2 in
             f 42 in
     y
  then let z = r := 4 in z
  else let u = r := 5 in u
 

;;;;
