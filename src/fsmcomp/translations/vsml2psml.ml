open Kast
open Ktypes 

let mk_idle l = 
  "idle_"^l

let mk_start l =
  "start_"^l

let mk_rdy l =
  "rdy_"^l

let set_multiple_atoms bs e =
  let set (x,a) e = PSML.Set(x,a, e) in
  List.fold_right set bs e

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
    | DoIn _ -> 
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
  type t = PSML.psm list * PSML.transition list * signature
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

let rec c_automaton ~i ~o ~idle ~start ~rdy ~result ~ty (ts,e) =
  let open PSML in
  let ((ps,ts,s'),e) = 
    let* ts' = list_map (fun (qxs,e) -> 
               let* e' = c_e idle result e in 
               ret (qxs,e')) ts in
    let* e' = c_e idle result e in
    let t = ((idle,[]),
             If(Atom.(bool_of_std_logic @@ mk_var start),
                Set(rdy,Atom.mk_std_logic' Zero,e'),
                Set(rdy,Atom.mk_std_logic' One,State(idle,[])))) in
    let* () = out ([],t::ts',[(i,start,Ktypes.(TConst TStd_logic));
                          (o,rdy,Ktypes.(TConst TStd_logic));
                          (o,result,ty)]) in
    run @@ ret (State(idle,[]))
  in
  (ps,s',(ts,e))

and c_e idle result e : PSML.exp m = 
  let open VSML in
  match e with
  | Atom a -> 
      ret @@ PSML.Set(result,a,State(idle,[]))
  | State(q,args) ->
      ret @@ PSML.State(q,args)
  | If(a,e1,e2) ->
      let* e1' = c_e idle result e1 in
      let* e2' = c_e idle result e2 in
      ret @@ PSML.If(a,e1',e2')
  | Continue a ->
      ret @@ PSML.Continue a
  | Case(a,hs,e) ->
      let* hs' = list_map (fun (c,e) -> 
                   let* e' = c_e idle result e in
                   ret (c,e')) hs in
      let* e' = c_e idle result e in
      ret @@ PSML.Case(a,hs',e')
  | DoIn(bs,e) ->
      let q = Gensym.gensym "q" in
      let* e' = c_e idle result e in 
      let t = (q,[]), e' in
      let* () = out ([],[t],[]) in
      ret @@ set_multiple_atoms bs @@ PSML.State(q,[])
  | LetIn(bs,e) when List.for_all (function (_,([],e)) -> is_atom e | _ -> false) bs ->
      let q = Gensym.gensym "q" in
      let* e' = c_e idle result e in
      let t = ((q,List.map fst bs),e') in
      let* () = out ([],[t],[]) in
      ret @@ PSML.State(q,List.map (fun (_,(_,e)) -> as_atom e) bs)
  | LetIn([((x,ty),(ts,e))],e2) ->
      let q = Gensym.gensym "q" in
      let* ts' = list_map (fun (qxs,e) -> 
                   let* e' = c_e q x e in 
                   ret (qxs,e')) ts in
      let* e' = c_e q x e in
      let* e2' = c_e idle result e2 in
      let t = (q,[]),e2' in
      let* () = out ([],t::ts',[(Local,x,ty)]) in
      ret e'
  | LetIn(bs,e) -> 
      let bsp,bsn = List.partition (fun (_,fsm) -> parallelisable fsm) bs in
      let bs_atom,bsp = List.partition (function (_,([],Atom _)) -> true | _ -> false) bsp in   
      let e2 = mk_let bs_atom (let_multiple bsn e) in
      match bsp with
      | [] -> 
          c_e idle result e2
      | [b] -> 
          c_e idle result (LetIn(bsp,e2))
      | ((x1,ty1),(ts1,e1))::bsp' -> 
        let ls = List.map (fun _ -> Gensym.gensym "l") bsp' in
        let* fsms' = list_map (fun (((x,ty),fsm), l) -> 
                         let (p,s,fsm) = c_automaton ~i:Local ~o:Local ~idle:(mk_idle l) ~start:(mk_start l) 
                                     ~rdy:(mk_rdy l) ~result:x ~ty fsm in
                        out(fsm::p,[],s))
                       (List.combine bsp' ls) in
        let q = Gensym.gensym "q" in
        let q1 = Gensym.gensym "p" in
        let q2 = Gensym.gensym "r" in
        let* ts1' = list_map (fun (qxs,e) -> 
                         let* e' = c_e q2 x1 e in
                         ret (qxs,e')) ts1 in
        let* e1' = c_e q2 x1 e1 in
        let* e2' = c_e idle result e2 in
        let t = ((q,[]), set_multiple_atoms (List.map (fun l -> (mk_start l, Atom.mk_std_logic' One)) ls) @@
                         State(q1,[])) in
        let t1 = ((q1,[]),set_multiple_atoms (List.map (fun l -> (mk_start l, Atom.mk_std_logic' Zero)) ls) @@ e1') in
        let t2 = ((q2,[]),PSML.If(all_rdy ls,e2',PSML.State(q2,[]))) in
        let* () = out ([],t::t1::t2::ts1',[(Local,x1,ty1)]) in
        ret @@ PSML.State(q,[])

let compile_vsml_circuit VSML.{x;s;ty;body} = 
  let (ps,s',fsm) = c_automaton ~i:In ~o:Out
                     ~idle:"idle" 
                     ~start:"start" 
                     ~rdy:"rdy" 
                     ~result:"result" 
                     ~ty body in
  PSML.{x;s=s@s'; body =fsm::ps}
