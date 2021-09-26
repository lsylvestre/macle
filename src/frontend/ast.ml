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
  | LetRec of ((ident * (ident * ty) list) * exp) list * exp
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

  let mk_let bs e ty = 
  match bs with
  | [] -> e 
  | _ -> Let(bs,e,ty)

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
