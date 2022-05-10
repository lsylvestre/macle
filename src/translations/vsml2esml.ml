open Ast
open Types
open TMACLE

let is_atom = Is_atom.is_atom

let translate_tconst (tc:tconst) : Esml.Typ.tconst =
  match tc with
  | TInt ->
      Esml.Typ.TInt
  | TBool ->
      Esml.Typ.TBool
  | TUnit ->
      Esml.Typ.TUnit

let rec translate_type (t:ty) : Esml.Typ.t =
  match t with
  | TConst c ->
      Esml.Typ.TConst (translate_tconst c)
  | TVar {contents=V n} ->
      Esml.Typ.TVar n
  | TVar {contents=Ty _} ->
      assert false (* canonize [t] before *)
  | TConstr (x,tys) ->
      Esml.Typ.TPtr(x,List.map translate_type tys)
  | TFun _ ->
      assert false (* functional value must be eliminated before *)
  | TPacket (ty,size) ->
      (match canon size with
       | TSize n ->
           Esml.Typ.TPacket (translate_type ty,n)
       | _ -> assert false (* todo: que faire si la taille n'est pas connue *)
     )
  | TSize _ ->
      assert false


(* convert a VSML atomic expression into an ESML atom *)
let rec as_atom (desc,ty) =
  let module A = Esml.Atom in

  let as_const = function
  | Bool b -> A.Bool b
  | Int n -> A.Int n
  | Cstr s -> A.Cstr s
  | EmptyList -> A.EmptyList
  | Unit -> A.Unit
  in
  let as_unop = function
  | Not -> A.Not
  | Uminus -> A.Uminus
  | DivBy2 -> A.DivBy2
  | Mod2 -> A.Mod2
  in
  let as_binop = function
  | Add -> A.Add
  | Sub -> A.Sub
  | Mul -> A.Mul
  | Le -> A.Le
  | Ge -> A.Ge
  | Lt -> A.Lt
  | Gt -> A.Gt
  | Eq -> A.Eq
  | Neq -> A.Neq
  in
  match desc with
  | Const c -> A.Const (as_const c)
  | Var x -> A.Var x
  | Unop(p,e) ->
      A.Prim(A.Unop(as_unop p),[as_atom e])
  | Binop(p,e1,e2) ->
      A.Prim(A.Binop(as_binop p),[as_atom e1;as_atom e2])
  | If(e1,e2,e3) ->
      A.Prim(If (translate_type (ty_of e2)),[as_atom e1;as_atom e2;as_atom e3])
  | PacketPrim c ->
      (match c with
      | PkMake es ->
          A.Prim(PkMake (translate_type ty), List.map as_atom es)
      | PkGet(e,idx) ->
          A.Prim(PkGet,[as_atom e;as_atom idx])
      | _ -> assert false (* already expanded *))
  | _ -> assert false (* not an atom *)


open Esml

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
     let e = ESML_continue q in
     match xs with
     | [] -> e
     | _ ->
        ESML_do(List.map2 (fun (x,_) a -> (x,a)) xs args, e)


let env_extend idle ts env =
  (idle,[])::List.map fst ts @ env

let set_multiple_atoms bs e =
  ESML_do(bs, e)

let let_multiple bs e =
  let let_ (x,e) e' = Let([(x,e)], e'),ty_of e' in
  List.fold_right let_ bs e

let all_rdy ls =
  Atom.mk_fold_binop Atom.And @@
  List.map (fun l -> Atom.(bool_of_std_logic @@ var_ (mk_rdy l))) ls

let rec parallelisable (e,_) =
  match e with
  | Var _ | Const _ | Unop _ | Binop _ ->
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
  | PacketPrim c ->
      (match c with
       | PkMake(es) ->
           List.for_all parallelisable es
       | PkGet(e,idx) ->
           parallelisable e && parallelisable idx
       | PkSet _
       | ToPacket _ 
       | OfPacket _ ->
           false
       | PkMap _ 
       | PkReduce _ 
       | PkScan _ ->
           assert false)
  | Macro _ ->
      assert false
  | StackPrim c ->
      (match c with
       | Push(_,e) -> 
           parallelisable e
       | Push_arg(e1,e2) ->
           parallelisable e1 && parallelisable e2
       | LetPop(_,e) ->
           parallelisable e
       | Save(_,e) ->
           parallelisable e
       | Restore -> 
           true
     ) 
  | _ -> false

(* *********************************** *)
open Monads
module M = struct
  type t = Esml.automaton list * Esml.transition list * Esml.signature
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
  let open Atom in
  let t = (q, (* [address] must not be free in [t] *)
           ESML_if(eq_ (var_ "avm_rm_waitrequest") std_zero,
                   ESML_do([("avm_rm_read",std_zero);
                            (result, k @@ Prim(FromCaml (translate_type ty),[var_ "avm_rm_readdata"]))],
                            ESML_continue idle),
                   ESML_continue q)) in
  let s = ESML_do([("avm_rm_address", compute_adress_ (var_ "caml_heap_base") address field);
                   ("avm_rm_read", std_one)],
                  ESML_continue q) in
  ([t],s)

let assign ~idle ~result ?(value=Atom.Const Unit) address ~field data tv =
  Esml2vhdl.allow_heap_assign := true;
  let q = Gensym.gensym "wait_write" in
  let open Esml in
  let open Atom in
  let t = (q, ESML_if(eq_ (var_ "avm_wm_waitrequest") std_zero,
                      ESML_do([("avm_wm_write",Atom.std_zero);
                               (result, value)],
                              ESML_continue idle),
                 ESML_continue q)) in
  let s = ESML_do([("avm_wm_address", Prim(ComputeAddress,[Var "caml_heap_base"; address ; field]));
                   ("avm_wm_writedata", Prim(ToCaml (translate_type tv),[data]));
                   ("avm_wm_write",Atom.std_one)],
                  ESML_continue q) in
  ([t],s)

let alloc ~idle ~result size hd tv = 
  Esml2vhdl.allow_heap_alloc := true;
  let open Esml in
  let open Atom in
  let q = Gensym.gensym "wait_write" in
  let t = (q, ESML_if(eq_ (var_ "alloc_rdy") std_one,
                      ESML_do([("alloc_rq", std_zero);
                               (result, Prim(ToCaml (translate_type tv),[var_ "alloc_ptr"]))],
                              ESML_continue idle),
                 ESML_continue q)) in
  let s = ESML_do(["alloc_rq", std_one; 
                   "alloc_size", size;
                   "alloc_tag", hd], ESML_continue q) in
  ([t],s)
(* 
  let automaton
    q -> 
      if alloc_rdy = '0' then 
        do dst := alloc_ptr 
      else 
        continue q 
  end in 
    do alloc_rq := true 
    and alloc_size := size 
    and alloc_hd := hd then 
    continue q

 *)


let case_bind address xs e0 =
  let rec aux ts q i = function
  | [] -> ESML_continue q, ts
  | (None,_)::xs ->
      aux ts q (i+1) xs
  | (Some x,ty)::xs ->
      let ts2,e2 = access ~idle:q ~result:x address ~field:(Atom.mk_int i) ~ty in
      let q2 = Gensym.gensym "q" in
      aux ((q2,e2)::ts2@ts) q2 (i+1) xs in
  let q = Gensym.gensym "q" in
  aux [q,e0] q 0 xs


let main_idle = "idle"

let rec c_automaton env ~i ~o ~idle ~start ~rdy ~result ~ty e =
  let open Esml in
  let ((ps,ts,s'),e) =
    let* e' = c_e env idle result e in
    let t = (idle,
             ESML_if(Atom.(bool_of_std_logic @@ var_ start),
                     ESML_do([rdy,Atom.std_zero],e'),
                     ESML_do([rdy,Atom.std_one],ESML_continue idle))) in
    let* () = out ([],[t],[(i,start,Typ.t_std_logic);
                          (o,rdy,Typ.t_std_logic);
                          (o,result,translate_type ty)]) in
    run @@ ret (ESML_continue idle)
  in
  (ps,s',(ts,e))

and c_e env idle result e : Esml.inst m =
  if is_atom e then
    let a = as_atom e in
    ret @@ ESML_do([result,a],ESML_continue idle)
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
      ret @@ ESML_if(as_atom a,e1',e2')
  | Match(e,cases) ->
      assert (is_atom e);
      let* cases' = list_map (fun (c,xs,e) ->
                      let* e' = c_e env idle result e in
                      let vars = List.filter_map (function
                                     | (Some x,ty) -> Some (Local,x,translate_type ty)
                                     | _ -> None) xs in
                      let* () = out ([],[],vars) in
                      ret (c,xs,e')) cases in
      let open Atom in
      let a = as_atom e in
      let* e1' = let r = ref [] in
          let rec aux ts n_constant = function
          | [] -> r := ts; ESML_continue idle
          | (c,[],e)::cases ->
              let e0,ts' = case_bind a [] e in
              ESML_if (eq_ a (Const (Cstr c)),
                       e0,
                       aux (ts'@ts) (n_constant+1) cases)
          | (c,xs,e)::cases ->
                (aux ts n_constant cases)
          in
          let e1 = aux [] 0 cases' in
          let* () = out ([],!r,[]) in
          ret @@ e1 in
      let* e2' =
        if List.for_all (function (_,[],_) -> true | _ -> false) cases then ret @@ ESML_continue idle else
        let x = Gensym.gensym "x" in
        let q = Gensym.gensym "q" in
        let ts2,e2 = access ~idle:q ~result:x a ~field:(Atom.mk_int (-1)) ~ty:(ty_of e) in
        let r = ref [] in
        let rec aux2 ts = function
        | [] -> r := ts; ESML_continue idle
        | ("::",xs,e)::_ ->
             (* optimisation in case of lists *)
             let e0,ts' = case_bind a xs e in
             r := (ts'@ts); e0
        | (c,[],e)::cases ->
              (aux2 ts cases)
        | (c,xs,e)::cases ->
            let e0,ts' = case_bind a xs e in
            (* debug : ESML_do([result,(Atom.Prim(TagHd,[Var x]))],ESML_continue idle) *)
            ESML_if (eq_ (Prim(TagHd,[Var x])) (Const (Cstr c)),
                     e0,
                     aux2 (ts'@ts) cases)
        in
        let e1 = aux2 [] cases' in
        let* () = out ([],(q,e1)::ts2 @ !r,[Local,x,translate_type (ty_of e)]) in
        ret @@ e2
      in
      ret @@ ESML_if(Prim(IsImm,[a]),e1',e2')
  | Raise exc ->
      Esml2vhdl.allow_trap := true;
      ret @@ ESML_do([("trap",Atom.mk_int 1)],ESML_continue main_idle)
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
                         ESML_continue q1) in
        let t1 = (q1,set_multiple_atoms (List.map (fun l -> (mk_start l, Atom.std_zero)) ls) @@ e1') in
        let t2 = (q2,ESML_if(all_rdy ls,e2',ESML_continue q2)) in
        let* () = out ([],[t;t1;t2],[(Local,x1,translate_type ty1)]) in
        ret @@ ESML_continue q
     )
     | CamlPrim p ->
        (match p with
        | RefAccess e0  ->
            assert (is_atom e0);
            let a = as_atom e0 in
            let ts,e' = access ~idle ~result a ~field:(Atom.mk_int 0) ~ty in
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
        | Ref e -> 
            assert (is_atom e);
            let open Atom in
            let q = Gensym.gensym "q" in
            let r = Gensym.gensym "r" in
            let tv = ty_of e in
            let ts,e1 = alloc ~idle:q ~result:r Atom.(mk_int 1) Atom.(mk_int 0) tv in
            let* () = out ([],ts,[]) in
            let data = as_atom e in
            let ts,e2 = assign ~idle ~result ~value:(Var r) (var_ r) ~field:Atom.(mk_int 0) data tv in
            let* () = out ([],(q,e2)::ts,[(Local,r,translate_type ty)]) in
            ret e1
        | ArrayMake{size;e} ->
          assert (is_atom size && is_atom e);
          let a = as_atom size in
          let ts,e' = alloc ~idle ~result a Atom.(mk_int 0) (ty_of e) in
          let* () = out ([],ts,[]) in
          ret e'
          (* todo remplir le tableau *)
     )
  | PacketPrim(PkSet(x,idx,e)) ->
      assert (is_atom idx && is_atom e);
      ret (ESML_PkSet((x,as_atom idx,as_atom e),ESML_continue idle))
      (* on pourrait assigner la valeur "()" à la destination *)
  | PacketPrim(ToPacket(a,idx,n)) ->
      assert (is_atom a && is_atom idx);
      Esml2vhdl.allow_heap_access := true;
      let q = Gensym.gensym "WaitRd" in
      let cnt = Gensym.gensym "cnt" in
      let open Atom in
      let ty = array_ty (ty_of a) in
      let t = (q,
               (ESML_if(eq_ (var_ "avm_rm_waitrequest") std_zero,
                  ESML_if(lt_ (var_ cnt) (mk_int n),
                               ESML_PkSet((result,var_ cnt,Prim(FromCaml (translate_type ty),[var_ "avm_rm_readdata"])),
                                          ESML_do([("avm_rm_address", compute_adress_ (var_ "caml_heap_base") (as_atom a) (add_ (as_atom idx) (add_ (var_ cnt) (mk_int 1))));
                                                    (cnt,add_ (var_ cnt) (mk_int 1))],
                                                  ESML_continue q)),

                                ESML_do([("avm_rm_read", std_zero);
                                                  (* (result, var_ data) *) ],
                   ESML_continue idle)),
                  ESML_continue q))) in
      let s = ESML_do([(result, Prim(PkCreate n,[mk_int 0]));
                       "avm_rm_address", compute_adress_ (var_ "caml_heap_base") (as_atom a) (as_atom idx);
                       "avm_rm_read",std_one;
                       cnt, (mk_int 0)],
                       ESML_continue q) in
      let* () = out ([],[t],[(Local,cnt,TConst TInt)]) in
      ret s
  (* [array_sub a idx n]
-- =======================================================
(Rd,    do data <= (0,0,0, ...)
        and avm_rm_address <= compute_address(e,idx)
        and avm_rm_read <= '1'
        and cnt <= 0 in
        continue WaitRd);
(WaitRd, if avm_rm_waitrequest = '0' then
            do data(cnt) <= unsigned(avm_rm_readdata) then
            (if cnt < n then
                do avm_rm_address <= std_logic_vector(raddr+4)
                and raddr <= raddr+4;
                and cnt <= cnt+1 then
                continue WaitRd
              else
                do avm_rm_read <= '0' then
                continue dst)
         else
           continue WaitRd)
-- ======================================================

*)
 | PacketPrim(OfPacket(a1,a2,idx,n)) ->
      assert (is_atom a1 && is_atom a2 && is_atom idx);
      Esml2vhdl.allow_heap_assign := true;
      let q = Gensym.gensym "WaitWrite" in
      let cnt = Gensym.gensym "cnt" in
      let open Atom in
      let ty = packet_ty (ty_of a1) in
      let t = (q,
               (ESML_if(eq_ (var_ "avm_wm_waitrequest") std_zero,
                  ESML_if(lt_ (var_ cnt) (mk_int n),
                                          ESML_do([("avm_wm_address", compute_adress_ (var_ "caml_heap_base") (as_atom a2) (add_ (as_atom idx) (var_ cnt)));
                                                   ("avm_wm_writedata",Prim(ToCaml (translate_type ty),[Prim(PkGet,[as_atom a1;var_ cnt])]));
                                                   (cnt,add_ (var_ cnt) (mk_int 1))],
                                                  ESML_continue q),
                                ESML_do([("avm_wm_write", std_zero);
                                         (result, Atom.Const Unit) ],
                   ESML_continue idle)),
                  ESML_continue q))) in
      let s = ESML_do(["avm_wm_address", compute_adress_ (var_ "caml_heap_base") (as_atom a2) (as_atom idx);
                       "avm_wm_writedata",Prim(ToCaml (translate_type ty),[Prim(PkGet,[as_atom a1;mk_int 0])]);
                       "avm_wm_write",std_one;
                       cnt, (mk_int 1)],
                       ESML_continue q) in
      let* () = out ([],[t],[(Local,cnt,TConst TInt)]) in
      ret s
  | PacketPrim _ ->
      assert false
  | StackPrim c ->
         (match c with
          | Push(xs,e) -> 
               let q = Gensym.gensym "q" in
               let* e' = c_e env idle result e in
               let t = (q,e') in
               let* () = out ([],[t],[]) in
               ret @@ ESML_stackPrim (Push(List.map (fun (x,t) -> (Atom.Var x,translate_type t)) xs,q))
          | Push_arg(a,e) -> 
               assert (is_atom a);
               let q = Gensym.gensym "q" in
               let* e' = c_e env idle result e in
               let t = (q,e') in
               let* () = out ([],[t],[]) in
               ret @@ ESML_stackPrim (Push([as_atom a,translate_type (ty_of a)],q))
          | LetPop(xs,e) -> 
               let q = Gensym.gensym "q" in
               let* e' = c_e env idle result e in
               let t = (q,e') in
               let xs' = List.map (fun (x,t) -> x,translate_type t) xs in
               let* () = out ([],[t],List.map (fun (x,t) -> Local,x,t) xs') in
               ret @@ ESML_stackPrim (LetPop(xs',q))
          | Save (q,e) -> 
               let q' = Gensym.gensym "q" in
               let* e' = c_e env idle result e in
               let t = (q',e') in
               let* () = out ([],[t],[]) in
               ret @@ ESML_stackPrim (Save (q,q'))
          | Restore ->
               ret @@ ESML_stackPrim Restore)
  | _ ->
      assert (is_atom e);
      assert false

let vsml2esml TMACLE.{x;xs;decoration;e} =
  let (ps,s',fsm) = c_automaton [] ~i:In ~o:Out
                     ~idle:main_idle
                     ~start:"start"
                     ~rdy:"rdy"
                     ~result:"result"
                     ~ty:decoration e in

  let extra = [] in
  let ptr = Typ.TPtr ("%private",[]) in
  let extra = if !Esml2vhdl.allow_heap_access
              || !Esml2vhdl.allow_heap_assign then
            (In,"caml_heap_base",ptr)::extra else extra in
  let extra = if !Esml2vhdl.allow_heap_access then
            (Out,"avm_rm_address",ptr)::
            (Out,"avm_rm_read",Typ.TConst TStd_logic)::
            (In,"avm_rm_waitrequest",Typ.TConst TStd_logic)::
            (In,"avm_rm_readdata",ptr)::extra else extra in
  let extra =
    if !Esml2vhdl.allow_heap_assign then
      (Out,"avm_wm_writedata", ptr)::
      (Out,"avm_wm_address",ptr)::
      (Out,"avm_wm_write",Typ.TConst TStd_logic)::
      (In,"avm_wm_waitrequest",Typ.TConst TStd_logic)::extra
    else extra in

   let extra =
    if !Esml2vhdl.allow_heap_alloc then
      (In,"alloc_rdy", Typ.TConst TStd_logic)::
      (Out,"alloc_rq", Typ.TConst TStd_logic)::
      (Out,"alloc_size",Typ.TConst TInt)::
      (Out,"alloc_tag",Typ.TConst TInt)::
      (In,"alloc_ptr",ptr)::extra
    else extra in

  let extra = if !Esml2vhdl.allow_trap
              then (Out,"trap",Typ.TConst TInt)::extra else extra in
  let vars = List.map (fun (x,t) -> (In,x,translate_type t)) xs @
              s' @
              extra in
  {x;vars; body =fsm::ps}
