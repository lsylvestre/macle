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
  | TVar{contents=V n} ->
      fprintf fmt "'a%d" n
  | TVar{contents=Ty t} ->
      fprintf fmt "(*tvar:=*)%a" print_ty t
  | TFun(ts,t) ->
      fprintf fmt "(";
      List.iter (fun ty -> print_ty fmt ty; fprintf fmt "->") ts;
      print_ty fmt t;
      fprintf fmt ")"
  | TFlatArray (t,size) ->
      fprintf fmt "(%a%a) array"
        print_ty t
        print_ty size
  | TSize n ->
      fprintf fmt "(*%d*)" n

let pp_ocaml_fun fmt xs pp e =
  fprintf fmt "(fun ";
  pp_print_list
    ~pp_sep:(fun fmt () -> fprintf fmt " ")
    (fun fmt (x,_) -> pp_ident fmt x) fmt xs;
  fprintf fmt "-> %a) " pp e

let pp_make_map fmt n =
  match n with
  | 0 -> assert false
  | 1 -> pp_print_text fmt  "Array.map"
  | 2 -> pp_print_text fmt  "Array.map2"
  | _ ->
    let open Gensym in
    let xs = List.init n (fun _ -> gensym "a") in
    let r = gensym "r" in
    let z = gensym "z" in
    let f = gensym "f" in
    let i = gensym "i" in (
    fprintf fmt "(fun %a " pp_ident f;
    pp_print_list
        ~pp_sep:(fun fmt () -> fprintf fmt " ") pp_ident fmt xs;
   fprintf fmt " ->@,@[<v 2>";
  fprintf fmt "let %a = Array.length %a in@,"
     pp_ident z
     pp_ident (List.hd xs);
   fprintf fmt "let %a = Array.make %a (%a "
     pp_ident r
     pp_ident z
     pp_ident f;
   pp_print_list
        ~pp_sep:(fun fmt () -> fprintf fmt " ")
        (fun fmt x -> fprintf fmt "%a.(0)" pp_ident x) fmt xs;
   fprintf fmt ") in@,";
   fprintf fmt "for %a = 1 to %a - 1 do@," pp_ident i pp_ident z;
   fprintf fmt "  %a.(%a) <- (%a " pp_ident r pp_ident i pp_ident f;
   pp_print_list
        ~pp_sep:(fun fmt () -> fprintf fmt " ")
        (fun fmt x -> fprintf fmt "%a.(%a)" pp_ident x pp_ident i) fmt xs;
   fprintf fmt ")@,done;@,%a@])" pp_ident r
  )

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
  | App (f,es) ->
      fprintf fmt "(%a " pp_ident f;
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
        | "::",[x;y] ->
            fprintf fmt "%a::%a -> %a" pp_pat x pp_pat y pp_exp e
        | _ ->
            fprintf fmt "%s%a -> %a" c pp_pats xs pp_exp e
      in
      fprintf fmt "(@[<v>match (%a : %a) with@," pp_exp e print_ty (ty_of e);
      pp_print_list
        ~pp_sep:(fun fmt () -> fprintf fmt "@,| ") pp_case fmt cases;
      fprintf fmt "| _ -> assert false";
      fprintf fmt ")@]";
  | Raise exc ->
      ( match exc with
        | Exception_Failure s ->
            fprintf fmt "(raise (Failure \"%s\"))" s
        | Exception_Invalid_arg s ->
            fprintf fmt "(raise (Invalid_argument \"%s\"))" s
      )
  | CamlPrim e ->
      ( match e with
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
      )
  | FlatArrayOp c ->
      ( match c with
        | FlatMake es ->
            fprintf fmt "[|";
            pp_print_list
              ~pp_sep:(fun fmt () -> fprintf fmt "; ") pp_exp fmt es;
            fprintf fmt "|]";
        | FlatGet {e;idx} ->
            fprintf fmt "(Array.get %a %a)" pp_exp e pp_exp idx
        | ArraySub(e,idx,n) ->
            fprintf fmt "(Array.sub %a %a %d)" pp_exp e pp_exp idx n
        | FlatMap((xs,e),es) ->
            fprintf fmt "(";
            pp_make_map fmt (List.length es);
            pp_ocaml_fun fmt xs pp_exp e;
            pp_print_list
              ~pp_sep:(fun fmt () -> fprintf fmt " ")
              pp_exp fmt es
        | FlatReduce ((acc,x,e0),init,e) ->
           fprintf fmt "(Array.fold_left";
           pp_ocaml_fun fmt [acc;x] pp_exp e0;
           fprintf fmt " %a %a)"
             pp_exp init
             pp_exp e
     )
  | Macro c ->
      ( match c with
        | LazyOr(e1,e2) ->
            fprintf fmt "(%a || %a)" pp_exp e1 pp_exp e2
        | LazyAnd(e1,e2) ->
            fprintf fmt "(%a && %a)" pp_exp e1 pp_exp e2
        | Map(f,es) ->
            pp_make_map fmt (List.length es);
            fprintf fmt " %a " pp_ident f;
            pp_print_list
              ~pp_sep:(fun fmt () -> fprintf fmt " ")
              pp_exp fmt es
        | Reduce (f,init,e) ->
            fprintf fmt "(Array.fold_left %a %a %a)"
              pp_ident f
              pp_exp init
              pp_exp e
        | ArrayUpdate {arr ; idx ; e } ->
            let x = Gensym.gensym "x" in
            fprintf fmt "(let %a = Array.copy %a in %a.(%a) <- %a; %a)"
              pp_ident x pp_exp arr
              pp_ident x pp_exp idx pp_exp e
              pp_ident x
        | OCamlArrayIterBy(n,f,e) ->
            let array_iter = Gensym.gensym "array_iter" in
            let i = Gensym.gensym "i" in
            let x = Gensym.gensym "x" in
            fprintf fmt "(let %a = %a in@," pp_ident x pp_exp e;
            fprintf fmt "let rec %a %a =@," pp_ident array_iter pp_ident i;
            fprintf fmt "if Array.length %a - %a < %d then invalid_arg \"array_iter_by\" else@," pp_ident x pp_ident i n;
            fprintf fmt "%a (Array.sub %a %a %d);@," pp_ident f pp_ident x pp_ident i n;
            fprintf fmt "if %a < Array.length  %a - %d then@," pp_ident i pp_ident x n;
            fprintf fmt "%a (%a+%d) in %a 0)" pp_ident array_iter pp_ident i n pp_ident array_iter
        | OCamlArrayReduceBy(n,f,init,e) -> (* à vérifier *)
            let array_iter = Gensym.gensym "array_iter" in
            let i = Gensym.gensym "i" in
            let acc = Gensym.gensym "acc" in
            let x = Gensym.gensym "x" in
            fprintf fmt "(let %a = %a in@," pp_ident x pp_exp e;
            fprintf fmt "let rec %a %a %a =@,"
              pp_ident array_iter pp_ident i pp_ident acc;
            fprintf fmt "if Array.length %a - %a < %d" pp_ident x pp_ident i n;
            fprintf fmt "then invalid_arg \"array_iter_by\" else@,";
            fprintf fmt "if %a >= Array.length  %a - %d then@,"
              pp_ident i pp_ident x n;
            fprintf fmt "%a else" pp_ident acc;
            fprintf fmt "%a " pp_ident array_iter;
            fprintf fmt "(%a %a (Array.sub %a %a %d))"
              pp_ident acc pp_ident f pp_ident x pp_ident i n;
            fprintf fmt "(%a+%d) in %a %a 0)"
              pp_ident i n pp_ident array_iter pp_exp init
        | OCamlArrayMapBy(n,x,e) ->
            let y = Gensym.gensym "y" in
            let z = Gensym.gensym "z" in
            let i = Gensym.gensym "i" in
            fprintf fmt "(let %a : %a = %a in@,"
              pp_ident y print_ty (ty_of e) pp_exp e;
            fprintf fmt "(Array.iteri ";
            fprintf fmt "(fun %a %a -> %a.(%a) <- %a (%a.(%a))) %a))"
              pp_ident i pp_ident z pp_ident y
              pp_ident i pp_ident x pp_ident y
              pp_ident i pp_ident y
        | OCamlArrayFoldLeft(x,acc,e) ->
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
