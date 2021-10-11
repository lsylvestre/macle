circuit doIn : sig input n:int, local r : int end return int = 
  let automaton 
      | A() -> 
          do r := 17 then
          do r := 18 then B(r)
      | B(x:int) -> x
      end 
  in
  A()

  