circuit sum (l) = 
  let rec aux (a,l) =
    if l = [] then a else aux(list_hd l + a, list_tl l) in
  aux(0,l)

;;;;
