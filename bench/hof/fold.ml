circuit main(l0) = 
  let fold_left(f,a0,l) =
    let rec aux(a,l2) = 
      if l2 = [] then a else aux(f(a,List.hd l2),List.tl l2) in aux(a0,l) in
  let plus(a,b) = a + b in
  fold_left(plus,0,l0)
;;;;

