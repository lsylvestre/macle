circuit f (x) = 
  raise (Failure "foo") 

;;;;

print_string (try f with Failure s -> s);;