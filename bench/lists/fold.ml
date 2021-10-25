circuit sum(l) =
  let plus(a,b) = a + b in
  list_fold_left(plus,0,l)

;;;;;;;

print_int @@ sum(l);;