open Kast

let rec compile_psml_circuit PSML.{x;s;body} = 
  let ss',fsms = Misc.split_map c_fsm body in
  ESML.{x;s=s@List.concat ss';body=fsms}

and c_fsm (ts,e) = 
  let env = List.map fst ts in
  let ts' = List.map (fun ((q,_),e) -> 
                q,c_e env e) ts in
  let e' = c_e env e in
  let locals = List.concat @@
               List.map (fun (q,xs) -> 
                          List.map (fun (x,t) -> mk_decl Local (x,t)) xs) env in
  locals,(ts',e')

and c_e env = function
| PSML.Continue a -> 
    ESML.Atom a
| PSML.Atom a -> 
    ESML.Atom a
| PSML.If(a,e1,e2) -> 
    let e1' = c_e env e1 
    and e2' = c_e env e2 in 
    ESML.If(a,e1',e2')
| PSML.Case(a,hs,e) -> 
    let cs,es = List.split hs in
    let e' = c_e env e in
    let es' = List.map (c_e env) es in
    let hs' = List.combine cs es' in
    ESML.Case(a,hs',e')
| PSML.DoThen(bs,e) -> 
    let e' = c_e env e in 
    ESML.DoThen(bs,e')
| PSML.State(q,args) ->
   (match List.assoc_opt q env with
   | None -> assert false
   | Some xs ->
     assert (List.compare_lengths xs args = 0);
     DoThen(List.map2 (fun (x,_) a -> (x,a)) xs args, 
            ESML.Atom (Atom.State q)))
