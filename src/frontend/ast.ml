open Loc
open Gensym

type ident = string
type state = string

open Types

open Atom

module TMACLE = struct

  type circuit = {
    x:ident;
    xs:(ident * ty) list;
    s:Kast.signature;
    ty:ty;
    e:exp 
  } 

  and exp =
  | Var of ident
  | Const of const
  | Prim of exp prim
  | If of (exp * exp * exp * ty)
  | Case of (exp * ty * (const * exp) list * exp * ty)  
  | App of (ident * exp list * ty)
  | LetRec of ((ident * (ident * ty) list) * exp) list * exp * ty
  | LetFun of ((ident * (ident * ty) list) * exp) * exp
  | Let of ((ident * ty) * exp) list * exp * ty
  | CamlPrim of interop
  
  and interop =
  | RefAccess of (exp * ty)
  | RefAssign of  { r:exp ; e:exp;ty:ty }
  | ArrayAccess of { arr:exp ; idx:exp; ty: ty }
  | ArrayAssign of { arr:exp ; idx:exp ; e:exp; ty:ty}
  | ArrayLength of (exp * ty)
  | ListHd of (exp * ty)
  | ListTl of (exp * ty)
  | ArrayMapBy of int * ident * ty * exp
  | ArrayFoldLeft of ident * ty * ty * exp * exp
  | ListFoldLeft of ident * ty * ty * exp * exp

  let t_unit = TConst TUnit
  let t_int = TConst TInt

  let mk_let ~ty bs e = 
  match bs with
  | [] -> e 
  | _ -> Let(bs,e,ty)

  let mk_let1 ~ty x e1 e2 =
    mk_let ~ty [(x,e1)] e2

  let mk_let_cascad ~ty bs e = 
    List.fold_right (fun b e -> mk_let ~ty [b] e) bs e

  let mk_seq ~ty e1 e2 =
    let x = Gensym.gensym "ignore" in
    mk_let1 ~ty (x,t_unit) e1 e2

  let rec mk_seqs ~ty es e =
     match es with
     | [] -> e
     | e::es' -> mk_seq ~ty e (mk_seqs ~ty es' e)

  let mk_letrec ~ty bs e =
    LetRec(bs,e,ty)

  let mk_letrec1 ~ty x args e1 e2 =
    mk_letrec ty [((x,args),e1)] e2

  let mk_if ~ty e1 e2 e3 = 
    If(e1,e2,e3,ty)

  let mk_app ~ty x args =
    App(x,args,ty)

  let mk_binop op e1 e2 = 
    Prim(Atom.Binop op,[e1;e2])

  let mk_int n = 
    Const (Int n)

  let mk_unit = 
    Const Unit

  let mk_empty_list = 
    Const EmptyList

  let mk_array_length ~ty e =
    CamlPrim (ArrayLength (e,ty))
  
  let mk_array_access arr idx ~ty_elem =
    CamlPrim (ArrayAccess {arr;idx;ty=ty_elem})
  
  let mk_array_assign arr idx e ~ty_elem =
    CamlPrim (ArrayAssign {arr;idx;e;ty=ty_elem})

  let mk_list_hd e ~ty_elem =
    CamlPrim(ListHd (e,ty_elem))

  let mk_list_tl e ~ty_elem =
    CamlPrim(ListTl (e,ty_elem))

end


module MACLE = struct
  
  type circuit = {
    x:ident; 
    xs:ident list; 
    e:exp;
    loc:loc
  }

  and exp = exp_desc located 
  
  and exp_desc =
  | Var of ident
  | Const of const
  | Prim of exp prim
  | If of (exp * exp * exp)
  | Case of (exp * (const * exp) list * exp)  
  | App of (ident * exp list)
  | LetRec of ((ident * ident list) * exp) list * exp
  | LetFun of ((ident * ident list) * exp) * exp
  | Let of (ident * exp) list * exp 
  | CamlPrim of interop
  
  and interop =
  | RefAccess of exp
  | RefAssign of  { r:exp ; e:exp }
  | ArrayAccess of { arr:exp ; idx:exp }
  | ArrayAssign of { arr:exp ; idx:exp ; e:exp}
  | ArrayLength of exp
  | ListHd of exp
  | ListTl of exp
  | ArrayMapBy of int * ident * exp
  | ArrayFoldLeft of ident * exp * exp
  | ListFoldLeft of ident * exp * exp

end

(** [mk_match loc e e1 x xs e2] builds the expression
    (match e with [] -> e1 | x::xs -> e2) expended :

  ~ (let #y = e in
     if #y = [] then e1 
     else let x = list_hd #y
          and xs = list_tl #y in
          e2)
*)
let mk_match loc e e1 x xs e2 =
  let open MACLE in
  let mkl = mk_loc loc in
  let y = Gensym.gensym "y" in
  let var_y = mkl @@ Var y in
  mkl @@ Let([(y,e)],
             mkl @@ If(mkl @@ Prim(Binop Eq,
                                   [ var_y;
                                     mkl @@ Const EmptyList ]),
                       e1,
                       mkl @@ Let([(x,mkl @@ CamlPrim (ListHd var_y));
                                   (xs,mkl @@ CamlPrim (ListTl var_y))],
                                   e2)))