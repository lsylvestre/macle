open Kast
open Ast
open Types 
open TMACLE

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
  let let_ (x,e) e' = Let([(x,e)], e'),ty_of e' in
  List.fold_right let_ bs e

let all_rdy ls =
  Atom.mk_fold_binop Atom.And @@
  List.map (fun l -> Atom.(bool_of_std_logic @@ var_ (mk_rdy l))) ls

let translate_tconst (tc:tconst) : Ktypes.tconst =
  match tc with
  | TInt ->
      Ktypes.TInt
  | TBool ->
      Ktypes.TBool
  | TStd_logic ->
      Ktypes.TStd_logic
  | TUnit ->
      Ktypes.TUnit

let rec translate_type (t:ty) : Ktypes.ty =
  match t with
  | TConst c -> 
      Ktypes.TConst (translate_tconst c)
  | TVar {contents=V n} -> 
      Ktypes.TVar n
  | TVar {contents=Ty _} -> 
      assert false (* canonize [t] before *)
  | TConstr (x,tys) -> 
      Ktypes.TPtr(x,List.map translate_type tys)
  | TPtr -> 
      Ktypes.TPtr("%private",[])
  | TFun _ -> 
      assert false (* functional value must be eliminated before *)

let rec parallelisable (e,_) =
  match e with
  | Var _ | Const _ | Prim _ -> 
      true
  | App(_,es) -> 
      List.for_all parallelisable es
  | Let(bs,e) -> 
      List.for_all (fun (_,e) -> parallelisable e) bs
      && parallelisable e
  | LetRec(bs,e) ->
      List.for_all (fun (_,e) -> parallelisable e) bs
      && parallelisable e
  | If(e1,e2,e3) ->
      parallelisable e1 && parallelisable e2 && parallelisable e3
  | Match(e,cases) ->
      parallelisable e &&
      List.for_all (fun (_,xs,e) -> List.length xs = 0 && parallelisable e) cases
  | _ -> false

let rec is_atom (e,_) = match e with
| Var _ | Const _ -> true
| Prim (p,es) -> List.for_all is_atom es
| _ -> false

let rec as_atom (e,_) = match e with
| Const c -> Atom.Const c
| Var x -> Atom.Var x
| Prim (p,es) -> Atom.Prim(p,List.map as_atom es)
| _ -> assert false



(* *********************************** *)
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


(* produit un code d'accès mémoire à une valeur [v] de type [tv]
   à l'adresse [address+field] retournant [k v]. *)
let access ?(k=fun a -> a) ~idle ~result address ~field ~ty = 
  Esml2vhdl.allow_heap_access := true;
  let q = Gensym.gensym "wait_read" in
  let open ESML in
  let open Atom in
  let t = (q, (* [address] must not be free in [t] *)
           if_ (eq_ (var_ "avm_rm_waitrequest") std_zero)
               (do_ [("avm_rm_read",std_zero);
                     (result, k @@ Prim(FromCaml (translate_type ty),[var_ "avm_rm_readdata"]))]
                     (state_ idle))
               (state_ q)) in
  let e = do_ [("avm_rm_address", compute_adress_ (var_ "caml_heap_base") address field);
               ("avm_rm_read", std_one)]
            (state_ q) in
  ([t],e)

let assign ~idle ~result address ~field data tv = 
  Esml2vhdl.allow_heap_assign := true;
  let q = Gensym.gensym "wait_write" in
  let open ESML in
  let open Atom in
  let t = (q, if_ (eq_ (var_ "avm_wm_waitrequest") std_zero)
                 (do_ [("avm_wm_write",Atom.std_zero);
                       (result, Const Unit)] 
                    (state_ idle))
                 (state_ q)) in
  let e = do_ [("avm_wm_address", Prim(ComputeAddress,[Var "caml_heap_base"; address ; field]));
               ("avm_wm_writedata", Prim(ToCaml (translate_type tv),[data]));
               ("avm_wm_write",Atom.std_one)]
            (state_ q) in
  ([t],e)

let case_bind address xs e0 =
  let rec aux ts q i = function
  | [] -> ESML.state_ q, ts
  | (None,_)::xs -> 
      aux ts q (i+1) xs
  | (Some x,ty)::xs ->
      let ts2,e2 = access ~idle:q ~result:x address ~field:(Atom.mk_int i) ~ty in
      let q2 = Gensym.gensym "q" in
      aux ((q2,e2)::ts2@ts) q2 (i+1) xs in
  let q = Gensym.gensym "q" in
  aux [q,e0] q 0 xs




let rec c_automaton env ~i ~o ~idle ~start ~rdy ~result ~ty e = 
  let open ESML in
  let ((ps,ts,s'),e) = 
    let* e' = c_e env idle result e in
    let t = (idle,
             If(Atom.(bool_of_std_logic @@ var_ start),
                DoThen([rdy,Atom.std_zero],e'),
                DoThen([rdy,Atom.std_one],Atom (State idle)))) in
    let* () = out ([],[t],[(i,start,Ktypes.t_std_logic);
                          (o,rdy,Ktypes.t_std_logic);
                          (o,result,translate_type ty)]) in
    run @@ ret (Atom (State idle))
  in
  (ps,s',(ts,e))

and c_e env idle result e : ESML.exp m =
  if is_atom e then 
    let a = as_atom e in
    ret @@ ESML.DoThen([result,a],Atom (State idle))
  else
  let (d,ty) = e in
  match d with
  | App(f,es) ->
      assert (List.for_all is_atom es);
      ret @@ app env f (List.map as_atom es) 
  | If(a,e1,e2) ->
      assert (is_atom a);
      let* e1' = c_e env idle result e1 in
      let* e2' = c_e env idle result e2 in
      ret @@ ESML.If(as_atom a,e1',e2')
  | Case(a,hs,e) ->
      assert (is_atom a);
      let* hs' = list_map (fun (c,e) -> 
                   let* e' = c_e env idle result e in
                   ret (c,e')) hs in
      let* e' = c_e env idle result e in
      ret @@ ESML.Case(as_atom a,hs',e')
  | Match(e,cases) -> 
      assert (is_atom e);
      let* cases' = list_map (fun (c,xs,e) -> 
                      let* e' = c_e env idle result e in
                      let vars = List.filter_map (function
                                     | (Some x,ty) -> Some (Local,x,translate_type ty)
                                     | _ -> None) xs in
                      let* () = out ([],[],vars) in
                      ret (c,xs,e')) cases in
      let open ESML in
      let open Atom in
      let a = as_atom e in
      let* e1' = let r = ref [] in
          let rec aux ts n_constant = function
          | [] -> r := ts; state_ idle
          | (c,[],e)::cases -> 
              let e0,ts' = case_bind a [] e in
              if_ (eq_ a (Const (Cstr c))) e0 
                (aux (ts'@ts) (n_constant+1) cases)
          | (c,xs,e)::cases -> 
                (aux ts n_constant cases) 
          in
          let e1 = aux [] 0 cases' in
          let* () = out ([],!r,[]) in
          ret @@ e1 in
      let* e2' =
        if List.for_all (function (_,[],_) -> true | _ -> false) cases then ret @@ state_ idle else
        let x = Gensym.gensym "x" in
        let q = Gensym.gensym "q" in
        let ts2,e2 = access ~idle:q ~result:x a ~field:(Atom.mk_int 0) ~ty:(ty_of e) in
        let r = ref [] in
        let rec aux2 ts = function
        | [] -> r := ts; state_ idle
        | (c,[],e)::cases -> 
              (aux2 ts cases)
        | (c,xs,e)::cases -> 
            let e0,ts' = case_bind a xs e in
            if_ (eq_ (Prim(TagHd,[Var x])) (Const (Cstr c))) e0 
              (aux2 (ts'@ts) cases) 
        in
        let e1 = aux2 [] cases' in
        let* () = out ([],(q,e1)::ts2 @ !r,[Local,x,translate_type (ty_of e)]) in
        ret @@ e2
      in 
      ret @@ (if_ (Prim(IsImm,[a])) e1' e2')
  | LetRec(ts,e) -> 
      let env' = List.map (fun ((x,args),_) -> (x,args)) ts @ env in
      let* ts' = 
        list_map (fun ((q,xs),e) -> 
                   let* () = out ([],[],List.map (fun (x,ty) -> Local,x,translate_type ty) xs)
                   in
                   let* e' = c_e env' idle result e in 
                   ret (q,e')
            ) ts in
      let* () = out ([],ts',[]) in   
      c_e env' idle result e
  | Let(bs,e) when List.for_all (function (_,e) -> is_atom e) bs ->
      let q = Gensym.gensym "q" in
      let env' = (q,List.map fst bs)::env in
      let* e' = c_e env' idle result e in
      let t = (q,e') in
      let* () = out ([],[t],[]) in
      let args = List.map (fun (_,e) -> as_atom e) bs in
      let* () = out ([],[],List.map (fun ((x,ty),_) -> Local,x,translate_type ty) bs) in
      ret @@ app env' q args
  | Let([((x,ty),e)],e2) ->
      let q_temp = Gensym.gensym "q" in
      let* e' = c_e env q_temp x e in
      let* e2' = c_e env idle result e2 in
      let t = q_temp,e2' in
      let* () = out ([],[t],[(Local,x,translate_type ty)]) in
      ret e'
  | Let(bs,e) -> 
     (let bsp,bsn = List.partition (fun (_,e) -> parallelisable e) bs in
      let bs_atom,bsp = List.partition (function (_,e) -> is_atom e) bsp in   
      let e2 = mk_let bs_atom (let_multiple bsn e) in
      match bsp with
      | [] ->
          c_e env idle result e2
      | [b] -> 
          c_e env idle result @@
          mk_let bsp e2
      | ((x1,ty1),e1)::bsp' -> 
        let ls = List.map (fun _ -> Gensym.gensym "l") bsp' in
        let q = Gensym.gensym "q" in
        let q1 = Gensym.gensym "p" in
        let q2 = Gensym.gensym "r" in
        let env' = (q,[])::(q1,[])::(q2,[])::env in
        let* fsms' = list_map (fun (((x,ty),e), l) -> 
                         let (p,s,fsm) = c_automaton env' ~i:Local ~o:Local ~idle:(mk_idle l) ~start:(mk_start l) 
                                     ~rdy:(mk_rdy l) ~result:x ~ty e in
                        out(fsm::p,[],s))
                       (List.combine bsp' ls) in
        let* e1' = c_e env q2 x1 e1 in
        let* e2' = c_e env' idle result e2 in
        let t = (q, set_multiple_atoms (List.map (fun l -> (mk_start l, Atom.std_one)) ls) @@
                         Atom (State q1)) in
        let t1 = (q1,set_multiple_atoms (List.map (fun l -> (mk_start l, Atom.std_zero)) ls) @@ e1') in
        let t2 = (q2,ESML.If(all_rdy ls,e2',Atom (State q2))) in
        let* () = out ([],[t;t1;t2],[(Local,x1,translate_type ty1)]) in
        ret @@ ESML.Atom (State q)
     )
     | CamlPrim p ->
        (match p with
        | RefAccess e0  ->
            assert (is_atom e0);
            let a = as_atom e0 in
            let ts,e' = access ~idle ~result a ~field:(Atom.mk_int 0) ~ty in
            let* () = out ([],ts,[]) in
            ret e'
        | ListHd e0 ->
            assert (is_atom e0);
            let a = as_atom e0 in
            let ts,e' = access ~idle ~result a ~field:(Atom.mk_int 0) ~ty in
            let* () = out ([],ts,[]) in
            ret e'
        | ListTl e0 ->
            assert (is_atom e0);
            let a = as_atom e0 in
            let ts,e' = access ~idle ~result a ~field:(Atom.mk_int 1) ~ty in
            let* () = out ([],ts,[]) in
            ret e'
        | ArrayLength e0 ->
            assert (is_atom e0);
            let a = as_atom e0 in
            let ts,e' = access ~k:(fun a -> Prim(SizeHeader,[a]))
                           ~idle ~result
                           a ~field:(Atom.mk_int (-1)) ~ty:(ty_of e0) in
            let* () = out ([],ts,[]) in
            ret e'
         | ArrayAccess{arr;idx} ->
            assert (is_atom arr && is_atom idx);
            let ts,e' = access ~idle ~result (as_atom arr) 
                          ~field:(as_atom idx) ~ty in
            let* () = out ([],ts,[]) in
            ret e'
        | RefAssign {r;e=data} ->
            assert (is_atom r && is_atom data);
            let ts,e' = assign ~idle ~result 
                               (as_atom r) ~field:(Atom.mk_int 0)
                               (as_atom data) (ty_of data) in
            let* () = out ([],ts,[]) in
            ret e' 
        | ArrayAssign{arr;idx;e=data} ->
            assert (is_atom arr && is_atom idx && is_atom data);
            let ts,e' = assign ~idle ~result 
                               (as_atom arr) ~field:(as_atom idx) 
                               (as_atom data) (ty_of data) in
            let* () = out ([],ts,[]) in
            ret e'                   
        | ( ArrayMapBy _ | ArrayFoldLeft _ | ListFoldLeft _) -> 
            assert false (* already expanded *)
     )
  | _ -> assert false (* atoms *)

let compile_circuit TMACLE.{x;xs;decoration;e} = 
  let (ps,s',fsm) = c_automaton [] ~i:In ~o:Out
                     ~idle:"idle"
                     ~start:"start" 
                     ~rdy:"rdy" 
                     ~result:"result" 
                     ~ty:decoration e in
  
  let extra = [] in
  let ptr = Ktypes.TPtr ("%private",[]) in
  let extra = if !Esml2vhdl.allow_heap_access
              || !Esml2vhdl.allow_heap_assign then
            (In,"caml_heap_base",ptr)::extra else extra in
  let extra = if !Esml2vhdl.allow_heap_access then 
            (Out,"avm_rm_address",ptr)::
            (Out,"avm_rm_read",Ktypes.TConst TStd_logic)::
            (In,"avm_rm_waitrequest",Ktypes.TConst TStd_logic)::
            (In,"avm_rm_readdata",ptr)::extra else extra in
  let extra =
    if !Esml2vhdl.allow_heap_assign then 
      (Out,"avm_wm_writedata", ptr)::
      (Out,"avm_wm_address",ptr)::
      (Out,"avm_wm_write",Ktypes.TConst TStd_logic)::
      (In,"avm_wm_waitrequest",Ktypes.TConst TStd_logic)::extra
    else extra in
  let s = List.map (fun (x,t) -> (In,x,translate_type t)) xs @ 
            s' @ 
            extra in
  ESML.{x;s; body =fsm::ps}
