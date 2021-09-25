circuit s(a,i,x) =
  let f(j) =
    a.(i+j) <- x
  in f(2)

;;;