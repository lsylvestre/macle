circuit fact : sig input a:int, input b:int end return int = 
  let automaton 
      | Main () ->
          let x : int =  
            let automaton 
                | Fact(acc:int,n:int) ->
                    if n <= 0 then acc else Fact(acc*n,n-1)
                end in Fact(1,a)
          and y : int =
            let automaton 
                | Fact(acc:int,n:int) -> 
                    if n <= 0 then acc else Fact(acc*n,n-1)
                end in Fact(1,b) 
          in
          x + y
      end 
  in Main()
