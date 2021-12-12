circuit length l = 
  let rec aux a l =
    match l with
    | [] -> a
    | _::t -> aux (1+a) t in
  aux 0 l
;;;;
