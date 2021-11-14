(* ******************************************************************* *)
(*                                                                     *)           
(*                Macle, an automata-based applicative language        *)
(*                  dedicated to FPGA-programming in OCaml             *)
(*                                                                     *)
(*                        (see the AUTHORS file)                       *)
(*                                                                     *)  
(* ******************************************************************* *)

type lang = PLATFORM

let inputs = ref ([] : string list) 

let add_file f = inputs := !inputs @ [f] 

let flag_lang = ref PLATFORM

let flag_verbose = ref false

let flag_vhdl_only = ref false
let flag_show_ast = ref false
let flag_show_typed_ast = ref false
let flag_show_inlined_ast = ref false

let flag_propagation = ref true (* optimisation *)

let flag_simulation_typed = ref false
let flag_simulation_inlined = ref false

let simulation_level = ref 0
let flag_vsml2esml = ref false

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
                            "display the typed AST\
                            \ of the input program");
      ("-show-inlined-ast", Arg.Set flag_show_inlined_ast, 
                            "display the typed AST\
                            \ of the input program\
                            \ after inlining");
      ("-no-propagation",   Arg.Clear flag_propagation, 
                            "disable constant/copy propagation");

      ("-verb",             Arg.Set flag_verbose, 
                            "print additionnal informations");
      ("-vsml2esml",        Arg.Set flag_vsml2esml, 
                            "alternative compilation scheme\
                            \ (from VSML to ESML)");
      ("-simul",            Arg.Set flag_simulation_typed,
                            "generate an OCaml program from typed AST");
      ("-simul-inlined-ast",Arg.Set flag_simulation_inlined,
                            "generate an OCaml program from inlined AST");
      
      ("-app",              set_lang PLATFORM,
                            "Macle input (default)");
      ("-vhdl-only",        Arg.Set flag_vhdl_only,
                            "generates a VHDL source without other\
                            \ files needed to extend an O2B platform") ] 
      
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

  if !flag_verbose then Printf.printf "\ninfos: process %s:\n" filename;

  try
    let lexbuf = Lexing.from_channel ic in
    let (circuits,main) = Parser.platform_macle Lexer.token lexbuf in    
    
    if !flag_simulation_typed || !flag_simulation_inlined then
      Ast.pprint_code_typ_constr_decl Format.std_formatter;

    List.iter
     (function c ->        
          (* initialize heap_access / heap_assign *)
          Esml2vhdl.allow_heap_access := false;
          Esml2vhdl.allow_heap_assign := false;
          
          if !flag_show_ast then
            Pprint_ast.PP_MACLE.pp_circuit Format.err_formatter c;

          let c = Typing.typing_circuit c in
          
          let c = Ast_rename.rename_ast c in

          if !flag_show_typed_ast then
            Pprint_ast.PP_TMACLE.pp_circuit Format.err_formatter c;

          if !flag_simulation_typed 
          then Ast2ocaml.pp_circuit Format.std_formatter c else

          let c = Macro_expansion.expand_circuit c in
          let c = Inline.inline_circuit c in
          

          (* let c = Ast2kast.compile_circuit c in 
          
          let c = Vsml_rename.rename_vsml_circuit c in

          if !flag_show_kast then  
            Pprint_kast.PP_VSML.pp_circuit Format.err_formatter c; 
   
          let c = Vsml_states_rename.rename_states_vsml_circuit c in *)
          let c = Ast2kernel.rewrite_circuit c in

          let c = if !flag_propagation 
                  then Propagation.constant_copy_propagation c 
                  else c 
          in

          let c = Let_floating.circuit_let_floating c in

          if !flag_show_inlined_ast then
            Pprint_ast.PP_TMACLE.pp_circuit Format.err_formatter c;
          
          if !flag_simulation_inlined
          then Ast2ocaml.pp_circuit Format.std_formatter c else
          
          let c = Ast_rename.rename_ast c in
          
          let c = Ast2esml.compile_circuit c (* if !flag_vsml2esml 
                  then Vsml2esml.compile_vsml_circuit c 
                  else let c = Vsml2psml.compile_vsml_circuit c in
                       Psml2esml.compile_psml_circuit c *)
          in
          mk_vhdl ~labels:false c
        ) circuits;
        
        let open Format in
        if !flag_simulation_typed || !flag_simulation_inlined then (
          fprintf std_formatter "@,%s" main;
          fprintf std_formatter ";;@]@."
        ) else (    
        let dst = "gen" in
        let app_name  = Filename.(concat dst (concat "apps" (name ^".ml"))) in
        let fmt = std_formatter in
        let app_oc = open_out app_name in
        set_formatter_out_channel app_oc;
        
        (* code added at the top of the file main.ml 
           of each application generated 
         *)
        fprintf fmt "@[<v>open Platform ;;@,@,";
        fprintf fmt "let print_int = Serial.write_int ;;@,";
        fprintf fmt "let print_string = Serial.write_string ;;@,@,";
        
        if !Gen_platform.flag_print_compute_time || 
           !Gen_platform.flag_print_compute_time_short then
          fprintf fmt "Timer.init() ;;@,@,";
        
        if !Gen_platform.flag_print_compute_time then 
         (fprintf fmt "let chrono f =@,";
          fprintf fmt "  let t1 = Timer.get_us () in@,";
          fprintf fmt "  let _ = f () in@,";
          fprintf fmt "  let t2 = Timer.get_us () in@,";
          fprintf fmt "  t2-t1@,@,");
        
        fprintf fmt "@[<b>";
        pp_print_text fmt main;
        fprintf fmt "@]@]@.";
        pp_print_flush fmt ();
        close_out app_oc
      );
     close_in ic
  with e -> 
    close_in ic; raise e ;;


let () =
  List.iter parse !inputs