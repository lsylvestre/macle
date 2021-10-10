(* ******************************************************************* *)
(*                                                                     *)           
(*                Macle, an automata-based applicative language        *)
(*                  dedicated to FPGA-programming in OCaml             *)
(*                                                                     *)
(*                        (see the AUTHORS file)                       *)
(*                                                                     *)  
(* ******************************************************************* *)

type lang = PLATFORM | VSML

let inputs = ref ([] : string list) 

let add_file f = inputs := !inputs @ [f] 

let flag_lang = ref PLATFORM

let flag_vhdl_only = ref false
let flag_show_ast = ref false
let flag_show_typed_ast = ref false
let flag_show_inlined_ast = ref false
let flag_show_kast = ref false

let flag_propagation = ref true (* optimisation *)

let flag_simulation = ref false

let () =
 let set_lang c = 
   Arg.Unit (fun () -> flag_lang := c)
 in
 Arg.parse [
      ("-time",            Arg.Set Gen_platform.flag_print_compute_time, 
                           "instrument the produced code to measure\
                           \ the compute time");
      ("-time-short",      Arg.Set Gen_platform.flag_print_compute_time_short,
                           "variant of the -time option");
      ("-show-ast",         Arg.Set flag_show_ast, 
                            "display the syntax tree (AST)\
                            \ of the input program");
      ("-show-typed-ast",   Arg.Set flag_show_typed_ast, 
                            "display the *typed* AST\
                            \ of the input program");
      ("-show-inlined-ast", Arg.Set flag_show_inlined_ast, 
                            "display the typed AST\
                            \ of the input program\
                            \ *after inlining*");
      ("-show-kast",        Arg.Set flag_show_kast, 
                            "display the *kernel AST* (VSML),\
                            \ an automaton-based representation");
      ("-no-propagation",   Arg.Clear flag_propagation, 
                            "disable constant/copy propagation");

      (* ************** *)
      ("-simul", Arg.Set flag_simulation,"generate an OCaml program");  
      ("-app",  set_lang PLATFORM,"PLATFORM");
      ("-vsml",  set_lang VSML,"VSML input file");
      ("-vhdl-only", Arg.Set flag_vhdl_only,
       "generates source files needed to extend an O2B platform")] 
      add_file "Usage:\n  ./compile files" 

let mk_vhdl ?labels ?(with_cc=false) c =
  if !flag_vhdl_only then 
    Esml2vhdl.compile_esml_circuit Format.std_formatter c
  else Gen_platform.mk_vhdl_with_cc ?labels c

let parse filename = 
  let ic = open_in filename in

  let name = Filename.(remove_extension @@ basename filename) in
  
  Esml2vhdl.allow_heap_access := false;
  Esml2vhdl.allow_heap_assign := false;

  Printf.printf "\n- process %s:\n" filename;
  try
    let lexbuf = Lexing.from_channel ic in
    (match !flag_lang with
    | VSML -> 
      Parser_vsml.vsml Lexer_vsml.token lexbuf
      |> Vsml_rename.rename_vsml_circuit
      |> Vsml_states_rename.rename_states_vsml_circuit
      |> Vsml2psml.compile_vsml_circuit
      |> Psml2esml.compile_psml_circuit
      |> (mk_vhdl)
    | PLATFORM -> 
        let (circuits,main) = Parser.platform_macle Lexer.token lexbuf in    
        if !flag_simulation then 
          let cs = List.map Typing.typing_circuit circuits in
          (List.iter (Ast2ocaml.pp_circuit Format.std_formatter) cs; exit 0)
        else 
        List.iter (function c ->        
                    (* initialize heap_access / heap_assign *)
                    Esml2vhdl.allow_heap_access := false;
                    Esml2vhdl.allow_heap_assign := false;
                    
                    if !flag_show_ast then
                      Pprint_ast.PP_MACLE.pp_circuit Format.err_formatter c;

                    let c = Typing.typing_circuit c in
                    let c = Ast_rename.rename_ast c in

                    if !flag_show_typed_ast then
                      Pprint_ast.PP_TMACLE.pp_circuit Format.err_formatter c;

                    let c = Inline.inline_circuit c in
                    let c = if !flag_propagation 
                            then Propagation.constant_copy_propagation c 
                            else c 
                    in
                    
                    if !flag_show_inlined_ast then
                      Pprint_ast.PP_TMACLE.pp_circuit Format.err_formatter c;
         
                    let c = Ast2kast.compile_circuit c in
                    
                    if !flag_show_kast then  
                      Pprint_kast.PP_VSML.pp_circuit Format.err_formatter c; 

                    c
                    |> Vsml_rename.rename_vsml_circuit
                    |> Vsml_states_rename.rename_states_vsml_circuit
                    |> Vsml2psml.compile_vsml_circuit
                    |> Psml2esml.compile_psml_circuit
                    |> (mk_vhdl ~labels:false)
        ) circuits;

        let open Format in
        let dst = "gen" in
        let app_name  = Filename.(concat dst (concat "apps" (name ^".ml"))) in
        let fmt = std_formatter in
        let app_oc = open_out app_name in
        set_formatter_out_channel app_oc;
        
        (* code added at the top of the file main.ml 
           of each application generated 
         *)
        fprintf fmt "open Platform ;;@,@,";
        fprintf fmt "let print_int = Serial.write_int ;;@,";
        fprintf fmt "let print_string = Serial.write_string ;;@,@,";

        if !Gen_platform.flag_print_compute_time 
           || !Gen_platform.flag_print_compute_time_short then
          fprintf fmt "Timer.init() ;;@,@,";
        pp_print_text fmt main;
        pp_print_flush fmt ();
        close_out app_oc
    );
     close_in ic
  with e -> 
    close_in ic; raise e ;;


let () =
  List.iter parse !inputs