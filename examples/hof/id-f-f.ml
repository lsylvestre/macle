circuit s n = 
  let id f = f in
  let inc x = x + 1 in
  let g = id inc in
  g 42
;;;;

