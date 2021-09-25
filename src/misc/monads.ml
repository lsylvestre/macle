module type MONAD = sig
  type 'a m
  val ret : 'a -> 'a m
  val (>>=) : 'a m -> ('a -> 'b m) -> 'b m
end

module Output (X : sig 
                     type t
                     val empty : t
                     val concat : t -> t -> t
                   end) :
  sig
      include MONAD with type 'a m = (X.t * 'a)
      val out : X.t -> unit m
      val run : 'a m -> X.t * 'a 
  end = 
  struct
    type 'a m = (X.t * 'a)
    let ret a = (X.empty,a)
    let (>>=) m k = 
      let (x,a) = m in
      let (y,b) = k a in
      (X.concat x y,b)
    let out x = (x,()) 
    let run m = m
  end
