circuit f x = 
  let rec h y = 
    let v = 
      let rec g x = 
        h x 
      in 42
    in 4
  in 2

(* in circuit f [line 5, characters 74-77]
   Error: The recursive call: h(x)
   should be in tail position. *)
   
;;;;