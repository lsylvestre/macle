type ident = string
type state = ident

type std_logic =  (* pas exposé dans le parser *)
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
| FromCaml of Ktypes.ty  (* usage interne, pas exposé dans le parser *)
| ToCaml of Ktypes.ty
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
