circuit f x = 
  let rec g x =
    if (let rec h y = h y in true) then 1 else 2
  in 
  42

   
;;;;