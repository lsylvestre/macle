circuit f x = 
  let rec g x =
    if (let rec h y = h y in g 4) then 1 else 2
  in 
  42

(* in circuit f [line 3, characters 60-63]
   Error: The recursive call: (g 4)
   should be in tail position *)
;;;;