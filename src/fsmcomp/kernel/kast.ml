open Loc

type ident = string
type state = ident

open Atom

type destination = In | Out | Local
type signature = (destination * ident * Ktypes.ty) list

type 'a product = 'a list

let mk_decl (d:destination) (x,ty) = (d,x,ty) 

module ESML = struct
  type circuit = {
    x:ident;
    s:signature;
    body: esm product
  }
  and esm = transition list * exp
  and transition = state * exp
  and exp =
  | Atom of atom
  | If of (atom * exp * exp)
  | DoThen of ((ident * atom) list * exp)
  | Case of (atom * (const * exp) list * exp)

  let if_ a e1 e2 = 
    If(a,e1,e2)

  let do_ bs e = 
    match bs with 
    | [] -> e
    | _ -> DoThen(bs,e)

  let state_ s = 
    Atom(State(s))
end
