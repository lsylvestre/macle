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
| EmptyList
| Unit

type op =   
| Binop of binop
| Unop of unop
| TyAnnot of Ktypes.ty
| FromCaml of Ktypes.ty  (* usage interne, pas exposé dans le parser *)
| ToCaml of Ktypes.ty
| CamlHeader
| CamlField of int
| CamlComputedField
| Size_hd

and atom = 
| Var of ident 
| Const of const
| State of state
| Prim of atom prim
and 'a prim = (op * 'a list)


(* smart constructors *)
let mk_bool (b:bool) = Bool b
let mk_int (n:int) = Int n
let mk_std_logic (v:std_logic) = Std_logic v
let mk_binop (p:binop) (a1:'a) (a2:'a) = (Binop p,[a1;a2])
let mk_unop (p:unop) (a:'a) = (Unop p,[a]) 
let mk_ty_annot (a:'a) (t:Ktypes.ty) = (TyAnnot t,[a])

let mk_prim p = Prim p
let mk_const c = Const c

let mk_not a =
  match a with 
  | Const(Bool b) -> Const(Bool (not b)) 
  | _ -> Prim (Unop Not,[a])

let mk_fold_binop p l =
  let op = Binop p in
  match l with
  | [] -> invalid_arg "mk_fold_binop"
  | [a] -> a
  | a::l -> List.fold_left (fun acc a -> Prim(op,[acc;a])) a l 

let mk_and a a' =
  match a,a' with 
  | Const(Bool true),a'' 
  | a'',Const(Bool true) 
      -> a''
  | (Const(Bool false) as f),_ 
  | _,(Const(Bool false) as f) -> f
  | _ -> Prim (Binop And,[a;a'])

let mk_bool' b         = mk_const @@ mk_bool b
let mk_int' n          = mk_const @@ mk_int n
let mk_std_logic' v    = mk_const @@ mk_std_logic v

let std_zero = mk_std_logic' Zero
let std_one = mk_std_logic' One

let mk_binop' p a1 a2  = mk_prim @@ mk_binop p a1 a2
let mk_unop' p a       = mk_prim @@ mk_unop p a
let mk_ty_annot' a t = mk_prim @@ mk_ty_annot a t

let mk_var x = Var x 

let bool_of_std_logic a = 
    mk_binop' Eq a (mk_std_logic' One)
