circuit f n a b = 
  let rec odd n = 
    if n <= 0 then false else even (n-1)
  and even n = 
    if n <= 0 then true else odd(n-1) 
  in
  if odd n then a else b

;;;;;


circuit sum_pow(x) =
  let rec f n = if n < 1 then 42 else f (n-1) in 
  let sum g n =
    let rec aux acc n = 
      if n < 0 then acc else 
      aux (acc+g(n)) (n-1) in
    aux 0 n in
  sum f x 

;;;;;

circuit sum_pow (n,k) =
  let pow(a,b) =
     let rec m(acc,a,b) =
       if b < 1 then acc else 
       m(acc*a,a,b-1) 
     in m(1,a,b)
  in
  let sum(f,n) =
    let rec aux(acc,n) = 
      if n < 0 then acc else 
      aux(acc+pow(n,k),n-1) in
    aux(0,n) 
  in
  sum(pow,n)

;;;;;


circuit sum_pow(n:int,k:int) =
  let automaton 
       aux(acc:int,n1:int) -> 
         if (n1 < 0) then acc else 
         let x1 : int =
            let automaton 
              m(acc1:int,a:int,b:int) ->
                if (b < 1) then acc1 else 
                m((acc1 * a), a, (b - 1))
            end in 
            m(1, n1, k)
         in
         let x2 : int = acc + x1 in
         aux(x1,n1 - 1)
      end in 
      aux(0, n)





circuit sum_pow(n : int, k : int) =
  (let rec aux#16(acc#17 : int, n#18 : int) = 
     if (n#18 < 0)
     then acc#17
     else let x#1d : int =
                (let rec w#19(acc#1a : int, a#1b : int, b#1c : int) = 
                   if (b#1c < 1)
                   then acc#1a
                   else w#19((acc#1a * a#1b), a#1b, (b#1c - 1))
                    in (w#19(1, n#18, k)
                    : int))
                in
                let x#1e : int =
                      (acc#17 + x#1d)
                in
                aux#16(x#1e, (n#18 - 1))
                
                 in (aux#16(0, n)
                 : int)) ;;  info: circuit  "sum_pow"  generated in folder gen/.






circuit sum_pow (n,k) =
  let automaton 
    Aux(acc:int,n :int) ->
      if n < 0 then acc else
      let x$1 : int = 
        let automaton
          f(acc:int, a:int,b:int) ->
            if b < 1 then acc else
            f(acc*a,a,b-1)
        end in f(1,n)
      and x$2 : int = n - 1 
      in
      let x$3 = acc + x$1 in
      aux(x3,)


(let rec aux#16(acc#17 : int, n#18 : int) = 
     if (n#18 < 0)
     then acc#17
     else let x#1d : int =
                (let rec f#19(acc#1a : int, a#1b : int, b#1c : int) = 
                   if (b#1c < 1)
                   then acc#1a
                   else f#19((acc#1a * a#1b), a#1b, (b#1c - 1))
                    in (f#19(1, n#18, k)
                    : int))
                 and x#1f : int =
                       (n#18 - 1)
                in
                let x#1e : int =
                      (acc#17 + x#1d)
                in
                aux#16(x#1e, x#1f)
                
                 in (aux#16(0, n)
                 : int)) ;;