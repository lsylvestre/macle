open Format

let pp_state = pp_print_text

open Pprint_atom

open Esml

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

