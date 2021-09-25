circuit fact : sig input n:int end return int = 
  let automaton 
      | Fact(acc:int,n:int) -> 
          if n <= 0 then acc else Fact(acc*n,n-1)
      end 
  in
  Fact(1,n)