circuit ackermann a b =
  let rec ack m n = 
  if m = 0 then n+1 else 
  if n = 0 then let x = ack (m-1) 1 in x else 
  let x1 = ack m (n-1) in
  let x2 = ack (m-1) x1 in
  x2 
in ack a b


;;;;

print_int (ackermann 3 2);;