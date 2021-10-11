open Kast
open PSML

(* renommage des paramètres des états d'un programme PSML *)

let gensym (x:ident) : ident = 
  Gensym.gensym x
   
let r_ident env x = 
  match List.assoc_opt x env with
  | None -> Printf.printf "*** : %s\n" x; assert false
  | Some x' -> x'

let rec rename_psml_circuit ({s;body} as c) =
  let env = List.map (fun (_,x,_) -> (x,x)) s in
  { c with body = List.map (r_automaton env) body }

and r_e env = function
| Atom a -> 
    Atom (r_a env a)
| If(a,e1,e2) -> 
    If(r_a env a,
       r_e env e1,
       r_e env e2)
| Case(a,hs,e) -> 
    Case(r_a env a,
         List.map (fun (c,e) -> (c,r_e env e)) hs,
         r_e env e)
| DoThen(bs,e) -> 
    DoThen(List.map (fun (x,a) -> x, r_a env a) bs, r_e env e)
| Continue a -> 
    Continue (r_a env a)
| State (q,args) ->
    State (q,List.map (r_a env) args)

and r_a env a = 
  let open Atom in
  match a with
  | Var x ->
     Var (r_ident env x)
  | Prim(p,args) ->
      Prim(p,List.map (r_a env) args)
  | _ -> a

and r_automaton env (ts,e) =
  let ts' = List.map 
            (fun ((q,xs),e) ->
              let xs' = List.map (fun (x,t) -> (gensym x, t)) xs in
              let env' = List.map2 (fun (x,_) (x',_) -> (x,x')) xs xs' @ env in
              ((q,xs'),r_e env' e))
            ts in
    let e' = r_e env e in
    (ts',e')





