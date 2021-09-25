let inputs = ref ([] : string list) 

let add_file f = inputs := !inputs @ [f] 

let flag_print_ast = ref false

type lang = ESML | PSML | VSML

let flag_lang = ref VSML
let flag_gen_cc = ref false
let flag_print_dsml = ref false

let () =
 let set_lang c = 
   Arg.Unit (fun () -> flag_lang := c)
 in
 Arg.parse [
      ("-time",Arg.Set Gen_platform.print_compute_time, "compute ellapsed time");
      ("-time-short",Arg.Set Gen_platform.print_compute_time_short, "compute ellapsed time");
      ("-esml", set_lang ESML,"ESML");
      ("-psml", set_lang PSML,"PSML");
      ("-vsml", set_lang VSML,"VSML");
      ("-pprint-dsml", Arg.Set flag_print_dsml, "DSML pretty printer");
      (* ************** *)
      ("-gen-cc", Arg.Set flag_gen_cc,
       "generates source files needed to extend an O2B platform")] 
      add_file "Usage:\n  ./compile files" 

let mk_vhdl ?labels ?(with_cc=false) name esml =

  (* let vars = Typing_efsm.typ_prog efsm in *)

  match !flag_gen_cc,!flag_lang with 
  | false,_ -> Esml2vhdl.c_prog ~name Format.std_formatter esml
  | true,(VSML) -> 
      Gen_platform.mk_vhdl_with_cc ?labels name esml
  | true,_ -> 
     Printf.printf "*** warning: platform generation ignored.\n";
     Printf.printf "To generate a platform, please give a CSM description"


let parse filename = 
  let ic = open_in filename in

  let name = Filename.(remove_extension @@ basename filename) in

  Esml2vhdl.allow_heap_access := false;
  Esml2vhdl.allow_heap_assign := false;

  try 
    let lexbuf = Lexing.from_channel ic in
    (match !flag_lang with
    | VSML -> 
        Parser_vsml.vsml Lexer_vsml.token lexbuf
        |> Vsml_rename.rename_vsml_circuit
        |> Vsml_states_rename.r
        |> Vsml2psml.compile_vsml_circuit
        |> Psml2esml.compile_psml_circuit
        |> (mk_vhdl name)
    );
     close_in ic
  with e -> 
    close_in ic; raise e ;;


let () =
  List.iter parse !inputs