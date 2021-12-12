circuit s arr =
  let plus a b = a + b in
  array_fold_left plus 0 arr

;;;