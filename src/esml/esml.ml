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
    body : automaton product
  }

  (* signature *)

  and signature = (destination * ident * Ty.t) list
  and destination = In | Out | Local

  (* body *)

  and 'a product = 'a list

  and automaton = transition list * inst

  and transition = state * inst

  and inst =
    | ESML_continue of state
    | ESML_if of (Atom.t * inst * inst)
    | ESML_do of ((ident * Atom.t) list * inst)
    | ESML_PkSet of (ident * Atom.t * Atom.t) * inst
    | ESML_stackPrim of stack_prim

  and stack_prim =
   | Push of (Atom.t * Ty.t) * state
   | LetPop of (ident * Ty.t) * state
   | Save of state * state
   | Restore


  let mk_decl (d:destination) (x,ty) =
    (d,x,ty)
end

module Typ = struct
  type ty =
    | TConst of tconst
    | TPtr of name * ty list   (* e.g. int list *)
    | TVar of tvar
    | TPacket of ty * int
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
    | If of Typ.ty
    | FromCaml of Typ.ty  (* usage interne, pas exposé dans le parser *)
    | ToCaml of Typ.ty
    | ComputeAddress
    | SizeHeader
    | TagHd
    | IsImm
    | NextField
    | PkGet
    | PkMake of Typ.ty
    | PkCreate of int
  and atom =
    | Var of ident
    | Const of const
    | State of state
    | Prim of atom prim
  and 'a prim = (op * 'a list)

  type t = atom

  (* smart constructors *)

  let mk_bool (b:bool) : atom =
    Const (Bool b)

  let mk_int (n:int) : atom =
    Const (Int n)

  let mk_std_logic (v:std_logic) : atom =
    Const (Std_logic v)

  let std_zero : atom =
    mk_std_logic Zero

  let std_one : atom =
    mk_std_logic One

  let mk_binop (p:binop) a1 a2 : atom =
    Prim (Binop p,[a1;a2])

  let mk_unop (p:unop) a : atom =
    Prim (Unop p,[a])

  let mk_fold_binop p l =
    let op = Binop p in
    match l with
    | [] -> invalid_arg "mk_fold_binop"
    | [a] -> a
    | a::l -> List.fold_left (fun acc a -> Prim(op,[acc;a])) a l

  let var_ (x:ident) : atom =
    Var x

  let eq_ a1 a2 : atom =
    mk_binop Eq a1 a2

  let and_ a1 a2 : atom =
    mk_binop And a1 a2

  let lt_ a1 a2 : atom =
    mk_binop Lt a1 a2

  let add_ a1 a2 : atom =
    mk_binop Add a1 a2

  let not_ a : atom =
    mk_unop Not a

  let compute_adress_ (h:atom) (addr:atom) (ofs:atom) : atom =
    Prim(ComputeAddress,[h ; addr ; ofs])

  let bool_of_std_logic a : atom =
    mk_binop Eq a (mk_std_logic One)

end

include ESML(Atom)(Typ)
