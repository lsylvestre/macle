circuit sum l = 
  let rec aux a l =
    match l with
    | [] -> a
    | x::l' -> aux (x+a) l' in
  aux 0 l

;;;;
