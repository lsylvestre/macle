circuit length (l) = 
  let rec aux (a,l) =
    if l = [] then a else aux (1+a,List.tl l) in
  aux(0,l)
;;;;
