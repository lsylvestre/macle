circuit sum (l) = 
  let rec aux (a,l) =
    if l = [] then a else aux (List.hd l+a,List.tl l) in
  aux(0,l)
;;;;
