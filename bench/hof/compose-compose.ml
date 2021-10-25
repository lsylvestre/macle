circuit c(k) = 
  let inc(m) = m + 1 in
  let dec(m) = m - 1 in
  let compose(f,g,x) = f(g(x)) in 
  let f(x) = compose(inc,dec,x) in
  compose(f,f,k)
;;;;

