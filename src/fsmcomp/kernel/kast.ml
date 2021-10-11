open Loc

type ident = string
type state = ident

open Atom

type destination = In | Out | Local
type signature = (destination * ident * Ktypes.ty) list

type 'a product = 'a list
(* type ('src,'exp) transition = ('src * 'exp)
type ('state,'exp) automaton = ('state,'exp) transition list * 'exp *)

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
end

module PSML = struct
  
  type circuit = {
    x:ident;
    s:signature;
    body: psm product
  }
  and psm = transition list * exp
  and transition = (state * (ident * Ktypes.ty) list) * exp
  and exp =
  | Atom of atom
  | If of (atom * exp * exp)
  | DoThen of ((ident * atom) list * exp)
  | State of (state * atom list)
  | Continue of atom
  | Case of (atom * (const * exp) list * exp)  
end

module VSML = struct
  type circuit = {
    x:ident;
    s:signature;
    ty:Ktypes.ty;
    body:vsm
  }
  and vsm = transition list * exp
  and transition = (state * (ident * Ktypes.ty) list) * exp
  and exp =
  | Atom of atom
  | If of (atom * exp * exp)
  | DoThen of ((ident * atom) list * exp)
  | State of (state * atom list)
  | Continue of atom
  | Case of (atom * (const * exp) list * exp)
  | LetIn of ((ident * Ktypes.ty) * vsm) list * exp

  let mk_let bs e = 
    match bs with
    | [] -> e 
    | _ -> LetIn(bs,e)
end
