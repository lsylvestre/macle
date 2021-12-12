circuit main l0 = 
  let fold_left f a0 l =
    let rec aux a l2 = 
      match l2 with 
      | [] -> a 
      | x::xs -> aux (f a x) xs in 
    aux a0 l in
  let plus a b = a + b in
  fold_left plus 0 l0
;;;;

