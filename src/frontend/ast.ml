open Loc
open Gensym

type ident = string
type state = string
type constr = ident 

open Types

open Atom

type predefined_exception = 
  | Exception_Failure of string
  | Exception_Invalid_arg of string


module Make(D : sig type decoration end) = struct

  type decoration = D.decoration

  type circuit = {
    x:ident;
    xs:(ident * decoration) list;
    decoration:decoration;
    e:exp
  } 

  and exp = exp_desc * decoration
  and exp_desc =
  | Var of ident
  | Const of const
  | Prim of exp prim
  | If of (exp * exp * exp)
  | Case of (exp * (const * exp) list * exp)  
  | App of (ident * exp list)
  | LetRec of ((ident * (ident * decoration) list) * exp) list * exp
  | LetFun of ((ident * (ident * decoration) list) * exp) * exp
  | Let of ((ident * decoration) * exp) list * exp
  | Match of exp * case list
  | Raise of predefined_exception
  | CamlPrim of interop
  
  and case = constr * (ident option * decoration) list * exp

  and interop =
  | RefAccess of exp
  | RefAssign of  { r:exp ; e:exp }
  | ArrayAccess of { arr:exp ; idx:exp }
  | ArrayAssign of { arr:exp ; idx:exp ; e:exp }
  | ArrayLength of exp
  | ListHd of exp
  | ListTl of exp
  | ArrayMapBy of int * ident * exp
  | ArrayFoldLeft of ident * exp * exp
  | ListFoldLeft of ident * exp * exp

end


module TMACLE = struct
  include Make(struct type decoration = ty end)

  let t_unit = TConst TUnit
  let t_int = TConst TInt
  let t_bool = TConst TBool

  let mk_exp e ty =
    e,ty

  let ty_of (_,ty) = ty 

  let desc_of (d,_) = d 
  
  let array_ty = function
  | TConstr("array",[t]) -> t
  | _ -> assert false

  let ref_ty = function
  | TConstr("ref",[t]) -> t
  | _ -> assert false

  let list_ty = function
  | TConstr("list",[t]) -> t
  | _ -> assert false

  let mk_let bs e = 
  match bs with
  | [] -> e 
  | _ -> Let(bs,e),ty_of e
  
  let mk_let1 x e1 e2 =
    mk_let [(x,e1)] e2

  let mk_let_cascad bs e = 
    List.fold_right (fun b e -> mk_let [b] e) bs e

  let mk_seq e1 e2 =
    let x = Gensym.gensym "ignore" in
    mk_let1 (x,t_unit) e1 e2

  let rec mk_seqs es e=
     match es with
     | [] -> e
     | e'::es' -> mk_seq e' (mk_seqs es' e)

  let mk_letrec bs e =
    LetRec(bs,e),ty_of e

  let mk_letrec1 x args e1 e2 =
    mk_letrec [((x,args),e1)] e2

  let mk_if e1 e2 e3 = 
    If(e1,e2,e3),ty_of e2

  let mk_app ~ty x args =
    App(x,args),ty

  let mk_binop ~ty op e1 e2 = 
    Prim(Atom.Binop op,[e1;e2]),ty

  let mk_int n = 
    Const (Int n),t_int

  let mk_unit = 
    Const Unit,t_unit

  let mk_empty_list = 
    Const EmptyList

  let mk_array_length e =
    CamlPrim (ArrayLength e),t_int
  
  let mk_array_access arr idx =
    match arr with
    | _,TConstr("array",[ty]) -> 
      CamlPrim (ArrayAccess {arr;idx}),ty
    | _ -> assert false
    
  let mk_array_assign arr idx e =
    CamlPrim (ArrayAssign {arr;idx;e}),t_unit

  let mk_list_hd e =
    match e with
    | _,TConstr("list",[ty]) ->
        CamlPrim(ListHd e),ty
    | _ -> assert false

  let mk_list_tl e =
    match e with
    | _,(TConstr("list",[ty]) as t) ->
      CamlPrim(ListTl e),t
    | _ -> assert false
end

module MACLE = Make(struct type decoration = loc end)

let list_typ_constr_decl : (ident * (ident * ty list) list) list ref = ref []

let add_code_typ_constr_decl x cs = 
    list_typ_constr_decl := !list_typ_constr_decl @ [x,cs]

let pprint_code_typ_constr_decl fmt = 
  let open Format in
  let pp fmt (x,cs) =
    let pp_constructor fmt = function
    | (c,[]) -> fprintf fmt "%s" c
    | (c,tys) ->
       fprintf fmt "%s of " c;
       pp_print_list 
        ~pp_sep:(fun fmt () -> fprintf fmt " * ") print_ty fmt tys
    in
    fprintf fmt "type %s = " x;
    pp_print_list 
        ~pp_sep:(fun fmt () -> fprintf fmt "@,| ") pp_constructor fmt cs;
in 
  fprintf fmt "@[<v>";
  List.iter (pp fmt) !list_typ_constr_decl;
  fprintf fmt "@,@,@]"
  (* code_typ_constr_decl := !code_typ_constr_decl ^ "type " ^ x " = " ^ 
  String.concat "|" @@ List.map (function (c,[]) -> c | (c,tys) -> c^" of "^ 
    String.concat " * " @@ List.map print_ty  *)


let env_constructor : (string * (string * int * ty list)) list ref =
  let v = Types.newvar () in
 ref (["[]",("list",0,[]);
       "::",("list",0,[v;list_ v])])

let add_constructor x ty n tys = 
  env_constructor := (x,(ty,n,tys))::!env_constructor

let typ_of_constructor x = 
  let ty,_,tys = List.assoc x !env_constructor in (ty,tys)

let val_of_constructor x = 
  let _,n,tys = List.assoc x !env_constructor in
  (n,tys)



(* struct

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
  | Match of exp * case list
  | CamlPrim of interop
  
  and case = constr * ident list * exp

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
*)
(** [mk_match loc e e1 x xs e2] builds the expression
    (match e with [] -> e1 | x::xs -> e2) expended :

  ~ (let #y = e in
     if #y = [] then e1 
     else let x = list_hd #y
          and xs = list_tl #y in
          e2)
*)
(* let mk_match loc e e1 x xs e2 =
  let open MACLE in
  let mkl = mk_loc loc in
  let y = Gensym.gensym "y" in
  let var_y = mkl @@ Var y in
  mkl @@ Let([((y,loc),e)],
             mkl @@ If(mkl @@ Prim(Binop Eq,
                                   [ var_y;
                                     mkl @@ Const EmptyList ]),
                       e1,
                       mkl @@ Let([(x,mkl @@ CamlPrim (ListHd var_y));
                                   (xs,mkl @@ CamlPrim (ListTl var_y))],
                                   e2))) *)