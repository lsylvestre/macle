open Kast
open Ktypes 

let mk_idle l = 
  "idle_"^l

let mk_start l =
  "start_"^l

let mk_rdy l =
  "rdy_"^l

let app env q args =
  match List.assoc_opt q env with
  | None -> 
      Printf.printf "*** %s\n" q; assert false
  | Some xs ->
     assert (List.compare_lengths xs args = 0);
     let e = ESML.Atom (Atom.State q) in
     match xs with
     | [] -> e
     | _ -> 
        ESML.DoThen(List.map2 (fun (x,_) a -> (x,a)) xs args, e)

let env_extend idle ts env = 
  (idle,[])::List.map fst ts @ env

let set_multiple_atoms bs e =
  ESML.DoThen(bs, e)

let let_multiple bs e =
  let let_ (x,fsm) e = VSML.LetIn([(x,fsm)], e) in
  List.fold_right let_ bs e

let all_rdy ls =
  Atom.mk_fold_binop Atom.And @@
  List.map (fun l -> Atom.(bool_of_std_logic @@ mk_var (mk_rdy l))) ls

let no_do fsm =
  let open VSML in
  let exception Found in
  try
    let rec check = function
    | (Atom _ | Continue _ | State _) -> 
        ()
    | If(_,e1,e2) -> 
        check e1; check e2
    | Case(_,hs,e) -> 
        List.iter (fun (_,e) -> check e) hs; check e
    | DoThen _ -> 
        raise Found 
    | LetIn (bs,e) ->
        List.iter (fun (_,fsm) -> check_fsm fsm) bs;
        check e
    and check_fsm (ts,e) =
      List.iter (fun (_,e) -> check e) ts; 
      check e 
    in
    check_fsm fsm;
    true
  with Found -> false


let parallelisable e =
  no_do e


let is_atom = function
| VSML.Atom _ -> true
| _ -> false

let as_atom = function
| VSML.Atom a -> a
| _ -> assert false


open Monads
module M = struct 
  type t = ESML.esm list * ESML.transition list * signature
  let empty : t = ([],[],[])
  let concat (l1,l2,l3) (l1',l2',l3') = (l1@l1',l2@l2',l3@l3')
end

open Output(M)

let ( let* ) m f = m >>= f 

let rec list_map f = function
| [] -> ret []
| (x::xs) -> let* v = f x in
             let* l = list_map f xs in 
             ret (v::l)

let rec c_automaton env ~i ~o ~idle ~start ~rdy ~result ~ty (ts,e) =
  let open ESML in
  let env' = env_extend idle ts env in
  let ((ps,ts,s'),e) = 
    let* ts' = 
      list_map (fun ((q,xs),e) -> 
                 let* () = out ([],[],List.map (fun (x,ty) -> Local,x,ty) xs)
                 in
                 let* e' = c_e env' idle result e in 
                 ret (q,e')
          ) ts in
    let* e' = c_e env' idle result e in
    let t = (idle,
             If(Atom.(bool_of_std_logic @@ mk_var start),
                DoThen([rdy,Atom.mk_std_logic' Zero],e'),
                DoThen([rdy,Atom.mk_std_logic' One],Atom (State idle)))) in
    let* () = out ([],t::ts',[(i,start,Ktypes.(TConst TStd_logic));
                          (o,rdy,Ktypes.(TConst TStd_logic));
                          (o,result,ty)]) in
    run @@ ret (Atom (State idle))
  in
  (ps,s',(ts,e))

and c_e env idle result e : ESML.exp m = 
  let open VSML in
  match e with
  | Atom a -> 
      ret @@ ESML.DoThen([result,a],Atom (State idle))
  | State(q,args) ->
      ret @@ app env q args
  | If(a,e1,e2) ->
      let* e1' = c_e env idle result e1 in
      let* e2' = c_e env idle result e2 in
      ret @@ ESML.If(a,e1',e2')
  | Continue a ->
      ret @@ ESML.Atom a
  | Case(a,hs,e) ->
      let* hs' = list_map (fun (c,e) -> 
                   let* e' = c_e env idle result e in
                   ret (c,e')) hs in
      let* e' = c_e env idle result e in
      ret @@ ESML.Case(a,hs',e')
  | DoThen(bs,e) ->
      let* e' = c_e env idle result e in 
      ret @@ ESML.DoThen(bs,e')
  | LetIn(bs,e) when List.for_all (function (_,([],e)) -> is_atom e | _ -> false) bs ->
      let q = Gensym.gensym "q" in
      let env' = (q,List.map fst bs)::env in
      let* e' = c_e env' idle result e in
      let t = (q,e') in
      let* () = out ([],[t],[]) in
      let args = List.map (fun (_,(_,e)) -> as_atom e) bs in
      let* () = out ([],[],List.map (fun ((x,ty),_) -> Local,x,ty) bs) in
      ret @@ app env' q args
  | LetIn([((x,ty),(ts,e))],e2) ->
      let q_temp = Gensym.gensym "q" in
      let env' = env_extend q_temp ts env in
      let* ts' = list_map (fun ((q0,xs),e) -> 
                   let* e' = c_e env' q_temp x e in 
                   let* () = out ([],[],List.map (fun (x,ty) -> Local,x,ty) xs) in
                   ret (q0,e')) ts in
      let* e' = c_e env' q_temp x e in
      let* e2' = c_e env' idle result e2 in
      let t = q_temp,e2' in
      let* () = out ([],t::ts',[(Local,x,ty)]) in
      ret e'
  | LetIn(bs,e) -> 
      let bsp,bsn = List.partition (fun (_,fsm) -> parallelisable fsm) bs in
      let bs_atom,bsp = List.partition (function (_,([],Atom _)) -> true | _ -> false) bsp in   
      let e2 = mk_let bs_atom (let_multiple bsn e) in
      match bsp with
      | [] ->
          c_e env idle result e2
      | [b] -> 
          c_e env idle result (LetIn(bsp,e2))
      | ((x1,ty1),(ts1,e1))::bsp' -> 
        let ls = List.map (fun _ -> Gensym.gensym "l") bsp' in
        
        let q = Gensym.gensym "q" in
        let q1 = Gensym.gensym "p" in
        let q2 = Gensym.gensym "r" in
        let env' = (q,[])::(q1,[])::(q2,[])::env in
        let* fsms' = list_map (fun (((x,ty),fsm), l) -> 
                         let (p,s,fsm) = c_automaton env' ~i:Local ~o:Local ~idle:(mk_idle l) ~start:(mk_start l) 
                                     ~rdy:(mk_rdy l) ~result:x ~ty fsm in
                        out(fsm::p,[],s))
                       (List.combine bsp' ls) in
        
        let env1 = List.map fst ts1 @ env' in
        let* ts1' = list_map (fun ((q,xs),e) -> 
                         let* () = out ([],[],List.map (fun (x,ty) -> Local,x,ty) xs) in
                         let* e' = c_e env1 q2 x1 e in
                         ret (q,e')) ts1 in
        let* e1' = c_e env1 q2 x1 e1 in
        
        let* e2' = c_e env' idle result e2 in
        let t = (q, set_multiple_atoms (List.map (fun l -> (mk_start l, Atom.mk_std_logic' One)) ls) @@
                         Atom (State q1)) in
        let t1 = (q1,set_multiple_atoms (List.map (fun l -> (mk_start l, Atom.mk_std_logic' Zero)) ls) @@ e1') in
        let t2 = (q2,ESML.If(all_rdy ls,e2',Atom (State q2))) in
        let* () = out ([],t::t1::t2::ts1',[(Local,x1,ty1)]) in
        ret @@ ESML.Atom (State q)

let compile_vsml_circuit VSML.{x;s;ty;body} = 
  let (ps,s',fsm) = c_automaton [] ~i:In ~o:Out
                     ~idle:"idle"
                     ~start:"start" 
                     ~rdy:"rdy" 
                     ~result:"result" 
                     ~ty body in
  ESML.{x;s=s@s'; body =fsm::ps}
