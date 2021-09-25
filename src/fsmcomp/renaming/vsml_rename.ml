open Kast
open VSML

open Gensym

(* renommage des paramètres des états et des liaisons locales 
   des programmes VSML *)
   
let r_ident env x = 
  match List.assoc_opt x env with
  | None -> Printf.printf "** %s\n\n" x; assert false
  | Some x' -> x'

let rec r_e env = function
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
| DoIn(bs,e) -> 
    let r_b env (x,a) = x, r_a env a in
    DoIn(List.map (r_b env) bs,r_e env e)
| Continue a -> 
    Continue (r_a env a)
| State (q,args) ->
    State (q,List.map (r_a env) args)
| LetIn(bs,e) ->
    let env_ext = List.map (fun ((x,_),_) -> x,gensym x) bs in
    let env' = env_ext @ env in
    let bs' = 
      let ren ((_,ty),fsm) (_,x') =
        let fsm' = r_automaton env' fsm in 
        ((x',ty),fsm')
      in
      List.map2 ren bs env_ext
    in
    LetIn(bs',r_e env' e)

and r_a env a = 
  let open Atom in
  match a with
  | Var x ->
      Var (r_ident env x)
  | Prim(p,args) ->
      Prim(p,List.map (r_a env) args)
  | _ -> a

and r_automaton env (ts,e) = 
  let ts' = 
    List.map 
      (fun ((q,xs),e) ->
      let xs' = List.map (fun (x,ty) -> gensym x,ty) xs in
      let env' = List.map2 (fun (x,_) (x',_) -> x,x') 
                   xs xs' 
                 @ env in
      ((q,xs'),r_e env' e))
    ts 
  in
  (ts',r_e env e)


let rename_vsml_circuit p = 
  let env = List.map (fun (_,x,_) -> x,x) p.s in
  { p with body = r_automaton env p.body } 
