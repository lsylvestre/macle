open Kast
open Gensym
open VSML
(* renommage des états d'un programme VSML *)
   
let r_state env q = 
  match List.assoc_opt q env with
  | None -> assert false
  | Some q' -> q'

let rec rename_states_vsml_circuit c =
  { c with body = r_fsm [] c.body }

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
| State (q,xs) -> 
    State (r_state env q,xs)
| Continue x -> 
    Continue x
| LetIn(bs,e) -> 
   LetIn (List.map (fun (xty,fsm) -> (xty,r_fsm env fsm)) bs, 
          r_e env e)

and r_a env a = 
  let open Atom in
  match a with
  | State q ->
      State (r_state env q)
  | Prim (f,args) -> 
      Prim (f, List.map (r_a env) args)
  | _ -> a

and r_fsm env (ts,e) =
  let env' = (List.map (fun ((f,_),_) -> (f,gensym f)) ts) @ env in
  let ts' = List.map (fun ((q,xs),e) ->
                          let q' = r_state env' q in
                          ((q',xs),r_e env' e)) ts in
  (ts',r_e env' e)
