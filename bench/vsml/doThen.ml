circuit doIn : sig input n:int, local r : int end return int = 
  let automaton 
      | A() -> 
          do r := 17 then B()
      | B() -> 42
      end 
  in
  A()

  