open Loc

type ident = string
type constr = ident

open Types

type predefined_exception =
  | Exception_Failure of string
  | Exception_Invalid_arg of string

type binop =
  | Add
  | Sub
  | Mul
  | Le
  | Ge
  | Lt
  | Gt
  | Eq
  | Neq

type unop =
  | Not
  | Uminus
  | DivBy2
  | Mod2

type const =
  | Bool of bool
  | Int of int
  | Cstr of string
  | EmptyList
  | Unit

(** [Make] AST of the Macle language.
    Each node of the Ast is decorated by a value of type [D.decoration],
    for instance, the location or the type of this node *)

module Make(D : sig type decoration end) = struct

  type decoration = D.decoration

  (* a circuit is a global function with a name [x], a sequence of arguments [xs]
     and a body [e] that is an expression *)
  type circuit = {
    x:ident;
    xs:(ident * decoration) list;
    decoration:decoration;
    e:exp
  }

  (** an expression is
      - a variable [x],
      - a constant [c],
      - the application of an unary operator [- e]
      - the application of an binary operator [e1 + e2]
      - a conditionnal [if e1 then e2 else e3]
      - a function application [x e1 ... en]
      - a declaration of mutually recursive functions
        [ let rec f1 xs1 = e1 and ... fn xsn = en in e]
      - a declaration of local function [let f xs = e1 in e2]
      - a declaration of values [let x1 = e1 and ... xn = en in e]
      - a patern matching [match e with C1 xs1 -> e1 | ... Cn xsn -> en]
      - an error raising [raise err]
      - the application of a primitive over a data structure
        -- [!e]
        -- [e1 := e2]
        -- [e1.(e2)]
        -- [e1.(e2) <- e3]
        -- [array_length e]
        -- [array_map_by n x e]
        -- [array_fold_left f e_init e] *)

  and exp = exp_desc * decoration
  and exp_desc =
    | Var of ident
    | Const of const
    | Unop of unop * exp
    | Binop of binop * exp * exp
    | StackPrim of stack_prim
    | If of exp * exp * exp
    | App of ident * exp list
    | LetRec of ((ident * (ident * decoration) list) * exp) list * exp
    | LetFun of ((ident * (ident * decoration) list) * exp) * exp
    | Let of ((ident * decoration) * exp) list * exp
    | Match of exp * case list
    | Raise of predefined_exception
    | CamlPrim of interop
    | PacketPrim of packet_prim
    | Macro of macro

  and stack_prim =
   | Push of (ident * decoration) list * exp
   | Push_arg of exp * exp
   | LetPop of (ident * decoration) list * exp
   | Save of ident * exp
   | Restore


  and case = constr * (ident option * decoration) list * exp

  and interop =
    | RefAccess of exp
    | RefAssign of  { r:exp ; e:exp }
    | Ref of exp
    | ArrayAccess of { arr:exp ; idx:exp }
    | ArrayAssign of { arr:exp ; idx:exp ; e:exp }
    | ArrayMake of { size:exp ; e:exp }
    | ArrayLength of exp

  and packet_prim =
    | PkMake of exp list
    | PkGet of exp * exp
    | PkSet of ident * exp * exp
    | ToPacket of (exp * exp * int)
    | OfPacket of (exp * exp * exp * int)
    | PkMap of ((ident * decoration) list * exp) * exp list
    | PkReduce of ((ident * decoration) * (ident * decoration) * exp) * exp * exp
    | PkScan of ((ident * decoration) * (ident * decoration) * exp) * exp * exp
  and macro =
    | LazyOr of exp * exp
    | LazyAnd of exp * exp
    | OCamlArrayMap of int * ident * exp * exp
    | OCamlArrayReduce of int * ident * exp * exp
    | OCamlArrayScan of int * ident * exp * exp * exp
end

(** [TMACLE] : typed AST of the Macle Language *)

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

  let packet_ty = function
    | TPacket(t,_) -> t
    | _ -> assert false

  let packet_size = function
    | TPacket(_,TSize n) -> n
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
    match bs with
    | [] -> e
    | _ -> LetRec(bs,e),ty_of e

  let mk_letrec1 x args e1 e2 =
    mk_letrec [((x,args),e1)] e2

  let mk_if e1 e2 e3 =
    If(e1,e2,e3),ty_of e2

  let mk_app ~ty x args =
    App(x,args),ty

  let mk_pk_map ~ty f e =
    let tyr = packet_ty (ty_of e) in
    let x = Gensym.gensym "x" in
    let body = mk_app ~ty:tyr f [(Var x,ty)] in
    PacketPrim(PkMap(([(x,ty)],body),[e])),packet_ tyr (TSize (packet_size (ty_of e)))

  let mk_pk_reduce f init e =
    let ty_element = packet_ty (ty_of e) in
    let ty_init = ty_of init in
    let x = Gensym.gensym "x" in
    let acc = Gensym.gensym "acc" in
    let body = mk_app ~ty:ty_init f [(Var acc,ty_init);(Var x,ty_element)] in
    PacketPrim(PkReduce(((acc,ty_init),(x,ty_element),body),init,e)),ty_init


  let mk_pk_scan f init e =
    let ty_element = packet_ty (ty_of e) in
    let ty_init = ty_of init in
    let x = Gensym.gensym "x" in
    let acc = Gensym.gensym "acc" in
    let body = mk_app ~ty:ty_init f [(Var acc,ty_init);(Var x,ty_element)] in

    PacketPrim(PkScan(((acc,ty_init),(x,ty_element),body),init,e)),packet_ ty_init (TSize (packet_size (ty_of e)))




  let mk_binop ~ty op e1 e2 =
    Binop (op,e1,e2),ty

  let mk_int n =
    Const (Int n),t_int

  let mk_bool b =
    Const (Bool b),t_bool

  let mk_unit =
    Const Unit,t_unit

  let mk_empty_list =
    Const EmptyList

  let mk_ref_access e =
    match e with
    | _,TConstr("ref",[ty]) ->
      CamlPrim (RefAccess e),ty
    | _ ->
        assert false

  let mk_ref_assign r e =
    CamlPrim (RefAssign {r;e}),t_unit

  let mk_array_length e =
    CamlPrim (ArrayLength e),t_int

  let mk_array_access arr idx =
    match arr with
    | _,TConstr("array",[ty]) ->
        CamlPrim (ArrayAccess {arr;idx}),ty
    | _ ->
        assert false

  let mk_array_assign arr idx e =
    CamlPrim (ArrayAssign {arr;idx;e}),t_unit

end

(** [TMACLE] : AST of the Macle Language annotated
    with locations in the source program *)

module MACLE = Make(struct type decoration = loc end)

let list_typ_constr_decl : (ident * (ident * ty list) list) list ref = ref []

let add_code_typ_constr_decl x cs =
  list_typ_constr_decl := !list_typ_constr_decl @ [x,cs]

let pprint_code_typ_constr_decl fmt =
  let open Format in
  let pp fmt (x,cs) =
    let pp_constructor fmt = function
      | (c,[]) ->
          fprintf fmt "%s" c
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
  let w1 = Types.newvar () in
  let w2 = Types.newvar () in
  ref (["[]",("list",0,[]);
        "::",("list",0,[v;list_ v]);
        ",",("list",0,[w1;w2])])

let add_constructor x ty n tys =
  env_constructor := (x,(ty,n,tys))::!env_constructor

let typ_of_constructor x =
  let ty,_,tys = List.assoc x !env_constructor in (ty,tys)

let val_of_constructor x =
  let _,n,tys = List.assoc x !env_constructor in
  (n,tys)

