open Format
open Ast
open Esml.Atom

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
    fprintf fmt "(#from_caml %a)" Pp_ktypes.print_ty ty
| (ToCaml ty) -> 
    fprintf fmt "(#to_caml %a)" Pp_ktypes.print_ty ty 
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

