circuit add(x) =
  match x with y,z -> y + z

;;;;

print_int @@ add(42,8) ;;