open Loc

type ident = string
type state = ident

(* parameterized by the syntaxe of atom and types *)

module ESML (Atom : sig type t end)
            (Ty : sig type t end) = struct
  
  (* circuit *)

  type circuit = {
    x    : ident;
    vars : signature;
    body : esm product
  }

  (* signature *)

  and signature = (destination * ident * Ty.t) list
  and destination = In | Out | Local

  (* body *)

  and 'a product = 'a list

  and esm = transition list * inst

  and transition = state * inst

  and inst =
  | ESML_atom of Atom.t
  | ESML_continue of state
  | ESML_if of (Atom.t * inst * inst)
  | ESML_do of ((ident * Atom.t) list * inst)

  let mk_decl (d:destination) (x,ty) = 
    (d,x,ty) 
end

module Typ = struct
  type ty = 
  | TConst of tconst
  | TPtr of name * ty list   (* e.g. int list *)
  | TVar of tvar
  and tconst = TStd_logic | TBool | TInt | TUnit
  and name = string
  and tvar = int

  type t = ty

  let t_std_logic = 
    TConst TStd_logic
end

module Atom = struct

  type std_logic = 
  | U
  | X
  | Zero
  | One
  | Z
  | W
  | L
  | H
  | Whatever

  type binop = 
  | Add | Sub | Mul 
  | Le | Ge | Lt | Gt 
  | Eq | Neq 
  | And | Or

  type unop = 
  | Not | Uminus | DivBy2 | Mod2

  type const = 
  | Std_logic of std_logic
  | Bool of bool 
  | Int of int
  | Cstr of string
  | EmptyList
  | Unit

  type op =   
  | Binop of binop
  | Unop of unop
  | FromCaml of Typ.ty  (* usage interne, pas exposé dans le parser *)
  | ToCaml of Typ.ty
  | ComputeAddress
  | SizeHeader
  | TagHd
  | IsImm

  and atom = 
  | Var of ident 
  | Const of const
  | State of state
  | Prim of atom prim

  and 'a prim = (op * 'a list)

  type t = atom

  (* smart constructors *)
  let mk_binop (p:binop) (a1:'a) (a2:'a) = (Binop p,[a1;a2])
  let mk_unop (p:unop) (a:'a) = (Unop p,[a]) 

  let mk_bool (b:bool)           = Const (Bool b)
  let mk_int (n:int)             = Const (Int n)
  let mk_std_logic (v:std_logic) = Const (Std_logic v)

  let std_zero : atom = mk_std_logic Zero
  let std_one : atom = mk_std_logic One

  let mk_binop' p a1 a2  = Prim (mk_binop p a1 a2)
  let mk_unop' p a       = Prim (mk_unop p a)

  let mk_fold_binop p l =
    let op = Binop p in
    match l with
    | [] -> invalid_arg "mk_fold_binop"
    | [a] -> a
    | a::l -> List.fold_left (fun acc a -> Prim(op,[acc;a])) a l 

  let var_ x = Var x 

  let eq_ a1 a2 : atom = mk_binop' Eq a1 a2
  let and_ a a' : atom = mk_binop' And a a'
  let not_ a : atom =    mk_unop' Not a

  let compute_adress_ h addr ofs  = 
    Prim(ComputeAddress,[h; addr;ofs])

  let bool_of_std_logic a = 
      mk_binop' Eq a (mk_std_logic One)

end

include ESML(Atom)(Typ)
