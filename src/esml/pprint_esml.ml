open Format

let pp_state = pp_print_text

open Esml
open Atom

(* types *)

let rec print_ty fmt ty = 
  let open Format in
  let open Typ in 
  match ty with
  | TConst c -> 
    (match c with
    | TStd_logic -> 
      pp_print_text fmt "std_logic"
    | TBool -> 
        pp_print_text fmt "bool"
    | TInt -> 
        pp_print_text fmt "int"
    | TUnit -> 
        pp_print_text fmt "unit")
  | TPtr (c,[]) ->
     pp_print_text fmt c
  | TPtr (c,tys) ->
     pp_print_text fmt "(";
     pp_print_list 
          ~pp_sep:(fun fmt () -> fprintf fmt ",") print_ty fmt tys;
     fprintf fmt ") %s" c;
  | TVar n -> 
      fprintf fmt "'a%d" n



(* atoms *)

let pp_ident = pp_print_text

let pp_std_logic fmt v = 
  pp_print_text fmt @@
  match v with
  | U -> "'U'"
  | X -> "'X'"
  | Zero -> "'0'"
  | One -> "'1'"
  | Z -> "'Z'"
  | W -> "'W'"
  | L -> "'L'"
  | H -> "'H'"
  | Whatever -> "'-'"

let pp_const fmt c =
  match c with 
  | Std_logic v -> 
      pp_std_logic fmt v
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
  | Eq -> "="
  | Neq -> "<>" 
  | And -> "&&" 
  | Or -> "||"

let pp_unop fmt p = 
  pp_print_text fmt @@
  match p with 
  | Not -> "not" 
  | Uminus -> "-"
  | DivBy2 -> "div_by_2"
  | Mod2 -> "mod2"

let pp_op fmt = function
| Unop p -> 
    pp_unop fmt p
| Binop p -> 
    pp_binop fmt p
| (FromCaml ty) -> 
    fprintf fmt "(#from_caml %a)" print_ty ty
| (ToCaml ty) -> 
    fprintf fmt "(#to_caml %a)" print_ty ty 
| ComputeAddress -> 
    pp_print_text fmt "#compute_address"
| SizeHeader ->
    fprintf fmt "#size_hd"
| TagHd ->
    fprintf fmt "#tag_hd"
| IsImm ->
    fprintf fmt "#is_imm"

let parenthesized ~paren fmt cb = 
  if paren then fprintf fmt "("; 
  cb (); 
  if paren then fprintf fmt ")" 

let pp_atom fmt a = 
  let rec pp_atom_aux ~paren fmt a = 
  match a with
  | Var x ->
      pp_print_text fmt x
  | State q -> 
      pp_print_text fmt q
  | Const c -> 
      pp_const fmt c
  | Prim (Binop p,[a1;a2]) ->
      parenthesized ~paren fmt @@ fun () ->
        fprintf fmt "%a %a %a"
          (pp_atom_aux ~paren:true) a1
          pp_binop p
          (pp_atom_aux ~paren:true) a2
  | Prim (Unop p,[a]) -> 
      parenthesized ~paren fmt @@ fun () ->
        fprintf fmt "%a %a"
          pp_unop p
          (pp_atom_aux ~paren:true) a
  | Prim(op,args) -> 
     pp_op fmt op;
     pp_print_text fmt "(";
     pp_print_list 
        ~pp_sep:(fun fmt () -> fprintf fmt ",") (pp_atom_aux ~paren:false) fmt args;
     pp_print_text fmt ")"
in
pp_atom_aux ~paren:false fmt a


(* instructions *)


let rec pp_instruction fmt e = 
  match e with
  | ESML_atom a -> 
      pp_atom fmt a
  | ESML_continue q ->
      fprintf fmt "(continue %a)" pp_state q
  | ESML_if (a,s1,s2) ->
    fprintf fmt "if %a@ then @[<hov>%a@]@ else @[<hov>%a@]@,"
      pp_atom a
      pp_instruction s1
      pp_instruction s2
  | ESML_do(bs,s) ->
      fprintf fmt "@[<v>do ";
      pp_print_list 
          ~pp_sep:(fun fmt () -> fprintf fmt "@, and ") 
           (fun fmt (x,a) -> 
               fprintf fmt "@[<v 2>%s :=@,%a@]" x pp_atom a) fmt bs;
       fprintf fmt "@ then@,%a" pp_instruction s
  
and pp_transition fmt (q,s) = 
  fprintf fmt "| %s" q; 
  fprintf fmt ") = @,";
  pp_instruction fmt s

and pp_automaton fmt (ts,s) = 
  fprintf fmt "let automaton@,"; 
  List.iter (pp_transition fmt) ts;
  fprintf fmt "end in "; 
  pp_instruction fmt s

let pp_circuit fmt {x;vars;body} =
  fprintf fmt "@[<v>@[<v 2>circuit %s : sig " x;
  fprintf fmt "end ="; 
  pp_print_list 
    ~pp_sep:(fun fmt () -> fprintf fmt "|| ") pp_automaton fmt body;
  fprintf fmt "@] ;;@,@,@]@."

