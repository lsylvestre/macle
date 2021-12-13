open Format
open Ast

open TMACLE
open Types

let pp_ident fmt x =
  pp_print_text fmt String.(concat "_0x" @@ split_on_char '#' x)

let pp_const fmt c =
  match c with 
  | Bool b ->
      fprintf fmt "%b" b
  | Int n ->
      fprintf fmt "%d" n
  | EmptyList ->
      pp_print_text fmt "[]"
  | Unit ->
      pp_print_text fmt "()"
  | Cstr c -> 
       pp_print_text fmt c

let pp_binop fmt p =
  pp_print_text fmt @@
  match p with 
  | Add -> "+" 
  | Sub -> "-"
  | Mul -> "*" 
  | Le -> "<=" 
  | Ge -> ">=" 
  | Lt -> "<"
  | Gt -> ">" 
  | Eq -> "=="  (* physical equality *)
  | Neq -> "!=" (* physical inequality *)
  | Or -> "||" 
  | And -> "&&"


let pp_unop fmt p = 
  pp_print_text fmt @@
  match p with 
  | Not -> "not" 
  | Uminus -> "-"
  | DivBy2 -> "div_by_2"
  | Mod2 -> "mod2"

let rec print_ty fmt ty = 
  let open Format in 
  match ty with
  | TConst tc -> 
    (match tc with
    | TStd_logic -> 
        pp_print_text fmt "std_logic"
    | TBool -> 
        pp_print_text fmt "bool"
    | TInt -> 
        pp_print_text fmt "int"
    | TUnit -> 
        pp_print_text fmt "unit")
    | TConstr (x,[]) -> 
       pp_print_text fmt x
    | TConstr (x,tys) ->
       fprintf fmt "(";
       pp_print_list 
        ~pp_sep:(fun fmt () -> fprintf fmt ", ") 
           print_ty fmt tys;
       fprintf fmt ") %s" x;
  | TPtr ->
      pp_print_text fmt "ptr"
  | TVar{contents=V n} -> 
      fprintf fmt "'a%d" n
  | TVar{contents=Ty t} -> 
      fprintf fmt "{tvar <- %a}" print_ty t
  | TFun(ts,t) ->
      fprintf fmt "(";
      List.iter (fun ty -> print_ty fmt ty; fprintf fmt "->") ts;  (* manque "*" *)
      print_ty fmt t;
      fprintf fmt ")"

let rec pp_exp fmt (e,ty) = 
  match e with
  | Var x -> 
      pp_ident fmt x
  | Const c -> 
      pp_const fmt c
  | Unop(Mod2,e) ->
      fprintf fmt "(%a mod 2)"
        pp_exp e
  | Unop(DivBy2,e) ->
      fprintf fmt "(%a / 2)"
        pp_exp e
  | Unop(p,e) ->
      fprintf fmt "(%a %a)"
        pp_unop p
        pp_exp e 
  | Binop (p,e1,e2) ->
      fprintf fmt "(%a %a %a)"
        pp_exp e1
        pp_binop p
        pp_exp e2
| If (e1,e2,e3) ->
    fprintf fmt "(if %a@ then @[<hov>%a@]@ else @[<hov>%a@])@,"
      pp_exp e1
      pp_exp e2
      pp_exp e3
| App (q,es) ->
    fprintf fmt "(%a " pp_ident q;
    pp_print_list 
      ~pp_sep:(fun fmt () -> fprintf fmt " ") pp_exp fmt es;
     fprintf fmt ")@,"
| Let(bs,e) -> 
    fprintf fmt "@[<v>(let ";
    pp_print_list 
        ~pp_sep:(fun fmt () -> fprintf fmt "@, and ") 
         (fun fmt ((x,ty),e) -> 
             fprintf fmt "%a : %a = %a" pp_ident x print_ty ty pp_exp e) fmt bs;
     fprintf fmt "@,@]@,in %a)" pp_exp e
| LetFun(t,e) ->
    fprintf fmt "(@[<v 2>let ";
    pp_transition fmt t;
    fprintf fmt "@ in ";
    pp_exp fmt e;
    fprintf fmt ")@]"
| LetRec(ts,e) ->
    fprintf fmt "(@[<v>@[<v 2>let rec ";
     pp_print_list 
        ~pp_sep:(fun fmt () -> fprintf fmt "@]@,@[<v 2>and ") 
       pp_transition fmt ts;
    fprintf fmt "@]in (";
    pp_exp fmt e;
    fprintf fmt " : %a" print_ty (ty_of e);
    fprintf fmt "))@]"
| Match(e,cases) -> 
    let pp_case fmt (c,xs,e) = 
      let pp_pat fmt = function
      | None,ty -> fprintf fmt "(_ : %a)" print_ty ty 
      | Some x,ty -> fprintf fmt "(%a : %a)" pp_ident x print_ty ty in
      let pp_pats fmt = function
      | [] -> ()
      | xs ->
        pp_print_text fmt "(";
        pp_print_list 
         ~pp_sep:(fun fmt () -> fprintf fmt ", ") pp_pat fmt xs;
        pp_print_text fmt ")" in
      match c,xs with
      | "::",[x;y] -> fprintf fmt "%a::%a -> %a" pp_pat x pp_pat y pp_exp e
      | _ -> fprintf fmt "%s%a -> %a" c pp_pats xs pp_exp e  
    in
    fprintf fmt "(@[<v>match (%a : %a) with@," pp_exp e print_ty (ty_of e);
    pp_print_list 
      ~pp_sep:(fun fmt () -> fprintf fmt "@,| ") pp_case fmt cases;
    fprintf fmt "| _ -> assert false";
    fprintf fmt ")@]";
| Raise exc -> 
    (match exc with
    | Exception_Failure s -> 
        fprintf fmt "(raise @@ Failure \"%s\")" s
    | Exception_Invalid_arg s -> 
        fprintf fmt "(raise @@ Invalid_arg \"%s\")" s)
| CamlPrim e -> 
(match e with
| RefAccess e -> 
    fprintf fmt "!(%a : %a)"
      pp_exp e print_ty (ty_of e)
| RefAssign {r;e} -> 
    fprintf fmt "(%a : %a) := (%a)"
      pp_exp r
      print_ty (ty_of r)
      pp_exp e 
| ArrayAccess{arr;idx} -> 
    fprintf fmt "(%a : %a).(%a)"
      pp_exp arr
      print_ty (ty_of arr)
      pp_exp idx
| ArrayAssign{arr;idx;e} -> 
    fprintf fmt "(%a : %a).(%a) <- (%a)"
      pp_exp arr
      print_ty (ty_of arr)
      pp_exp idx
      pp_exp e
| ArrayLength e ->
    fprintf fmt "(Array.length (%a : %a))"
      pp_exp e
      print_ty (ty_of e)
| ArrayMapBy(n,x,e) ->
   let y = Gensym.gensym "y" in
   let z = Gensym.gensym "z" in
   let i = Gensym.gensym "i" in
   fprintf fmt "(let %a : %a = %a in@," pp_ident y print_ty (ty_of e) pp_exp e;
   fprintf fmt "(Array.iteri (fun %a %a -> %a.(%a) <- %a (%a.(%a))) %a))" 
     pp_ident i pp_ident z pp_ident y 
     pp_ident i pp_ident x pp_ident y 
     pp_ident i pp_ident y
| ArrayFoldLeft(x,acc,e) ->
    fprintf fmt "(Array.fold_left %a (%a : %a) (%a : %a))" 
      pp_ident x 
      pp_exp acc 
      print_ty (ty_of acc)
      pp_exp e
      print_ty (ty_of e)
)
and pp_transition fmt ((q,xs),e) = 
  fprintf fmt "@[<b>%a@ " pp_ident q; 
  pp_print_list 
    ~pp_sep:(fun fmt () -> fprintf fmt "@ ") 
       pp_tyconstr fmt xs;
  fprintf fmt " =@]@,";
  pp_exp fmt e;

and pp_tyconstr fmt (x,ty) = 
  fprintf fmt "(%a : %a)" pp_ident x print_ty ty

let pp_circuit fmt {x;xs;e} =
  fprintf fmt "@[<v 2>@[<b>let %a@ " pp_ident x;
  pp_print_list 
    ~pp_sep:(fun fmt () -> fprintf fmt "@ ") pp_tyconstr fmt xs;
  fprintf fmt "@ @]=@,";
  pp_exp fmt e;
  fprintf fmt "@ ;;@,@]@,"