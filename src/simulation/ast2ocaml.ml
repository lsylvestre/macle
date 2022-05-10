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
  | TPacket (t,size) ->
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
        | Ref e ->
            fprintf fmt "(ref (%a : %a))"
              pp_exp e print_ty (ty_of e)
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
        | ArrayMake{size;e} ->
            fprintf fmt "(Array.make %a (%a : %a))"
              pp_exp size
              pp_exp e
              print_ty (ty_of e)
      )
  | PacketPrim c ->
      ( match c with
        | PkMake es ->
            fprintf fmt "[|";
            pp_print_list
              ~pp_sep:(fun fmt () -> fprintf fmt "; ") pp_exp fmt es;
            fprintf fmt "|]";
        | PkGet (e,idx) ->
            fprintf fmt "(Array.get %a %a)" pp_exp e pp_exp idx
        | PkSet(x,idx,e) ->
            fprintf fmt "(Array.set %a %a %a)" pp_ident x pp_exp idx pp_exp e
        | ToPacket(e,idx,n) ->
            fprintf fmt "(Array.sub %a %a %d)" pp_exp e pp_exp idx n
        | OfPacket(e1,idx,e2,n) ->
            fprintf fmt "(Array.blit %a 0 %a %a %d)" pp_exp e1 pp_exp idx pp_exp e2 n
        | PkMap((xs,e),es) ->
            fprintf fmt "(";
            pp_make_map fmt (List.length es);
            pp_ocaml_fun fmt xs pp_exp e;
            pp_print_list
              ~pp_sep:(fun fmt () -> fprintf fmt " ")
              pp_exp fmt es
        | PkReduce ((acc,x,e0),init,e) ->
           fprintf fmt "(Array.fold_left";
           pp_ocaml_fun fmt [acc;x] pp_exp e0;
           fprintf fmt " %a %a)"
             pp_exp init
             pp_exp e
        | PkScan ((acc,x,e0),init,e) ->
           fprintf fmt "(let scan f init a =
   let dst = Array.make (Array.length a) init in
   let r = ref init in
   for i = 0 to Array.length a -1 do
     r := f !r a.(i);
     dst.(i) <- !r
   done;
   dst in scan ";
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
        | OCamlArrayMap(n,f,src,dst) ->
            let x_src = Gensym.gensym "x" in
            let y = Gensym.gensym "y" in
            let x_dst = Gensym.gensym "x_dst" in
            let i = Gensym.gensym "i" in
            fprintf fmt "(let %a : %a = %a in@,"
              pp_ident y print_ty (ty_of src) pp_exp src;
            fprintf fmt "(let %a : %a = %a in@,"
              pp_ident y print_ty (ty_of dst) pp_exp dst;
            fprintf fmt "(Array.iteri(*%d*) " n;
            fprintf fmt "(fun %a %a -> %a.(%a) <- %a %a) %a))"
              pp_ident i pp_ident x_src 
              pp_ident y pp_ident i 
              pp_ident f pp_ident x_dst
              pp_ident y
        | OCamlArrayReduce(n,f,acc,e) ->
            fprintf fmt "(Array.fold_left(*%d*) %a (%a : %a) (%a : %a))" n
              pp_ident f
              pp_exp acc
              print_ty (ty_of acc)
              pp_exp e
              print_ty (ty_of e)
        | OCamlArrayScan(n,f,init,e,dst) ->
            fprintf fmt "(let scan init a dst =
   let r = ref init in
     for i = 0 to Array.length a -1 do
       r := %a !r a.(i);
       dst.(i) <- !r
     done
   in scan " pp_ident f;
           fprintf fmt " %a %a %a)"
             pp_exp init
             pp_exp e
             pp_exp dst
      )
  | StackPrim(c) ->
      (match c with
      | LetPop(xs,e) -> 
          fprintf fmt "(";
          List.iter (fun (x,_) -> fprintf fmt "let %a = pop() in@," pp_ident x) xs;
          pp_exp fmt e;
          fprintf fmt ")"
      | Push(xs,e) -> 
         fprintf fmt "("; 
          List.iter (fun (x,_) -> fprintf fmt "push %a; " pp_ident x) xs;
          fprintf fmt " %a)" pp_exp e
      | Push_arg(e1,e2) -> 
          fprintf fmt "(push %a; %a)" pp_exp e1 pp_exp e2
      | Save(q,e) -> 
          fprintf fmt "(save %a; %a)" pp_ident q pp_exp e
      | Restore ->
          fprintf fmt "(restore())")

and pp_transition fmt ((q,xs),e) =
  fprintf fmt "@[<b>%a@ " pp_ident q;
  (match xs with 
  | [] -> 
    fprintf fmt "()";
  | _ -> 
     pp_print_list
       ~pp_sep:(fun fmt () -> fprintf fmt "@ ")
       pp_tyconstr fmt xs);
  fprintf fmt " =@]@,";
  pp_exp fmt e;

and pp_tyconstr fmt (x,ty) =
  fprintf fmt "(%a : %a)" pp_ident x print_ty ty

let pp_circuit fmt {x;xs;e} =
  (* **************************** *)
  if !Esml2vhdl.allow_stack then (
    fprintf fmt "@[<v>let stack_env__ : Obj.t list ref = ref []@,";
    fprintf fmt "@[<v>let stack_trace__ : (unit -> Obj.t) list ref = ref []@,";
    fprintf fmt "let pop () =@,";
    fprintf fmt "  match !stack_env__ with@,";
    fprintf fmt "  | n::t -> stack_env__ := t; (Obj.obj n)@,";
    fprintf fmt "  | _ -> assert false@,@,";

    fprintf fmt "let push n =@,";
    fprintf fmt "  stack_env__ := (Obj.repr n)::!stack_env__@,@,";

    fprintf fmt "let save f =@,";
    fprintf fmt "  stack_trace__ := (fun () -> Obj.repr (f()))::!stack_trace__@,@,";

    fprintf fmt "let restore () =@,";
    fprintf fmt "  match !stack_trace__ with@,";
    fprintf fmt "  | f::t -> stack_trace__ := t; Obj.obj (f())@,";
    fprintf fmt "  | _ -> assert false@,@,");
  (* **************************** *)
  fprintf fmt "@[<v 2>@[<b>let %a@ " pp_ident x;
  pp_print_list
    ~pp_sep:(fun fmt () -> fprintf fmt "@ ") pp_tyconstr fmt xs;
  fprintf fmt "@ @]=@,";
  pp_exp fmt e;
  fprintf fmt "@ ;;@,@]@,"
