circuit calcul r = 
  let x = !r in
  if let y = let rec f x = 
                let k = !r in
                if !r = 3 then true else f x 
             in 
             let h = !r in
             f 42 in
     y
  then let z = !r in z
  else let u = !r in u
 

;;;;
