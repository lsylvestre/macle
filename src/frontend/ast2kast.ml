open Kast
open Ast
open Types
open TMACLE
    
let translate_tconst = function
  | TInt ->
      Ktypes.TInt
  | TBool ->
      Ktypes.TBool
  | TStd_logic ->
      Ktypes.TStd_logic
  | TUnit ->
      Ktypes.TUnit

let rec translate_type t =
  match t with
  | TConst c -> 
      Ktypes.TConst (translate_tconst c)
  | TVar {contents=V n} -> 
      Ktypes.TVar n
  | TVar {contents=Ty _} -> 
      assert false (* canonize [t] before *)
  | TCamlArray ty -> 
      Ktypes.TPtr("array",[translate_type ty])
  | TCamlList ty -> 
      Ktypes.TPtr("list",[translate_type ty])
  | TCamlRef ty -> 
      Ktypes.TPtr("ref",[translate_type ty])
  | TPtr -> 
      Ktypes.TPtr("%private",[])
  | TFun _ -> 
      assert false (* functional value must be eliminated before *)

  let rec is_atom (desc,_) = match desc with
  | Var _ | Const _ -> true
  | Prim (_,args) ->
      List.for_all is_atom args  
  | _ -> false

(* [exp_to_atom e] raise [Assert_failure] if [e] is not an atom *)
let rec exp_to_atom (desc,_) = match desc with
| Var x -> 
    Atom.Var x
| Const n -> 
    Atom.Const n
| Prim (c,args) -> 
    Prim(c,List.map exp_to_atom args) 
| _ -> 
    assert false (* not an atom *)


let new_local_signal : (ident * Types.ty) list ref = ref []

let access ?(callbk=fun x -> x) ts tv p args = 
  assert (List.compare_lengths ts args = 0);
  let xs = List.map (fun t -> Gensym.gensym "tmp",translate_type t) ts in
  Esml2vhdl.allow_heap_access := true;
  let get = Gensym.gensym "get" in
  let read = Gensym.gensym "read" in
  let v = Gensym.gensym "tmp_access" in
  new_local_signal := (v,tv) :: !new_local_signal;
  let open VSML in
  ([((get,xs),
     DoThen([("avm_rm_address",
            Prim(p, (List.map (fun (x,_) -> Atom.Var x) xs)));
           ("avm_rm_read", Const (Std_logic One))],
          State(read,[])));
    ((read,[]),
     If (Prim(Binop Eq,[Var "avm_rm_waitrequest";Const (Std_logic Zero)]),
         DoThen ([("avm_rm_read",Const (Std_logic Zero));
                (v,Prim(FromCaml (translate_type tv),[Var "avm_rm_readdata"]))],
               (Atom (callbk (Atom.Var v)))),
         State(read,[])))],
   State(get,args))

let assign tt ts p args new_exp = 
  (* Efsm2vhdl.allow_heap_access := true; *)
  Esml2vhdl.allow_heap_assign := true;
  let eval = Gensym.gensym "eval" in
  let write = Gensym.gensym "write" in
  let x = Gensym.gensym "tmp" in 
  let xs = List.map (fun _ -> Gensym.gensym "tmp") args in 
  let ktt = translate_type tt in
  let open VSML in
  ([((eval,(x,ktt)::(List.map2 (fun x t -> (x,translate_type t)) xs ts)),
     DoThen([("avm_wm_writedata",
            Prim(ToCaml ktt,[Var x]));
           ("avm_wm_address",
            Prim(p,List.map (fun x -> Atom.Var x) xs));
           ("avm_wm_write", 
            Const (Std_logic One))],State(write,[])));
    ((write,[]),
     If (Prim(Binop Eq,[Var "avm_wm_waitrequest";Const (Std_logic Zero)]),
         DoThen([("avm_wm_write",Const (Std_logic Zero))],
              Atom(Const Unit)),
         State(write,[])))],
   State(eval,new_exp::args))

(* translate expressions *)
let rec translate_exp env e = 
  if is_atom e then [],VSML.Atom (exp_to_atom e) else
  let desc,ty = e in 
  match desc with
  | Var _ | Const _ -> 
      assert false (* is an atom *)
  | Prim (c,es) ->
      (match Typing.typ_prim c with
       | Types.TFun(tys,t) ->
          let xs = List.map (fun ty -> Gensym.gensym "tmp",ty) tys in
          let bs = List.combine xs es in
          translate_exp env @@
          (Let(bs,(Prim (c,List.map (fun (x,ty) -> Var x,ty) xs),t)),t)
      | _ -> assert false)
  | Let(bs,e) -> 
      let bs' =
        List.map (fun ((x,ty),e) -> 
                   ((x,translate_type ty),translate_exp env e)) bs 
      in
      let fsm = translate_exp env e in
      (match fsm with
       (* ici, c'est important de gérer le cas où [fsm] 
          est de la forme [([],e)].
          Par ex, à partir de [([],App(Var f,[a1;...an]))],
          - on veut produire [([],App(Var f,[a1;...an]))] 
          - et non [([],Let([(Var res,App(Var f,[a1;...an]))],Var res))]
            qui n'est pas un programme correct, puisque [f] "s'échappe" 
            de la liaison [res = f(a1,...an)]. *)
      | [],e -> [],LetIn(bs',e)
      | _ -> 
         let res = Gensym.gensym "res" in
         [],LetIn(bs',LetIn([(res,translate_type (ty_of e)),fsm],Atom(Atom.Var res))))
  | LetFun _ -> 
      (* non-recursive functions habe been systematically inlined before *)
      assert false 
  | LetRec(bs,e) ->
      let ty = ty_of e in
      let env' = List.map fst bs @ env in
      let ts =
        List.map (fun ((q,xs),e) -> 
            let xs' = List.map (fun (x,t) -> (x,translate_type t)) xs in
            let fsm = translate_exp env' e in
            let e' = match fsm with
                     (* fsm est de la forme [(ts,e')] avec
                        [ts] des "fonctions" locales dans lesquelles
                        il peut y avoir des occurrences des xs' 
                        (libres dans les ts).

                        C'est pourquoi on doit conserver la hiérarchie 
                        au niveau VSML.

                        Optimisation : si [ts] est vide, on peut aplatir.
                        *) 
                     | [],e' -> e'
                     | _ -> 
                        let res = Gensym.gensym "res" in
                        VSML.LetIn([(res,translate_type ty),fsm],
                                   Atom(Atom.Var res))
            in ((q,xs'),e')
          ) bs 
      in
      let (ts',e') = translate_exp env' e in
      (ts@ts',e')
  | If(e1,e2,e3) -> 
      let ty = ty_of e2 in
      if is_atom e1
      then let ts2,e2' = translate_exp env e2 in
           let ts3,e3' = translate_exp env e3 in
           ts2@ts3,VSML.If(exp_to_atom e1,e2',e3')
      else 
        translate_exp env @@
        let x = Gensym.gensym "dsl_cond" in
        Let([((x,TConst TBool),e1)], (If((Var x,TConst TBool),e2,e3),ty)),ty
  | Case(e1,hs,e2) ->  (* TODO *)
      let ty = ty_of e1 in
      let ty2 = ty_of e2 in
      if is_atom e1 
      then let tts,es = Misc.split_map (fun (_,e) -> translate_exp env e) hs in
           let ts2,e2' = translate_exp env e2 in
           ts2@List.concat tts, 
           VSML.Case(exp_to_atom e1,
                     List.map2 (fun (c,_) e -> (c,e)) hs es,
                     e2')
      else 
        translate_exp env @@
        let x = Gensym.gensym "dsl_cond" in
        (Let([((x,ty),e1)], (Case((Var x,ty),hs,e2),ty2)),ty2)
  | App(x,es) -> 
      if List.for_all is_atom es then
        [], VSML.State(x,List.map exp_to_atom es)
      else 
        (match List.assoc_opt x env with
        | None -> assert false
        | Some xs -> 
           translate_exp env @@
           (Let(List.combine xs es,
               (App(x,List.map (fun (x,ty) -> Var x,ty) xs),ty)),ty))
  | CamlPrim e ->
    (match e with
    | RefAccess e ->
        let ty_elem = ref_ty (ty_of e) in 
        let x = Gensym.gensym "x" in
        let ts,e' = 
          access [TPtr] ty_elem (CamlField 0) [Atom.Var x] in
        ts,VSML.LetIn([((x,translate_type ty), translate_exp env e)], e')
    | ArrayAccess{arr;idx} ->
        let ty_elem = array_ty (ty_of arr) in
        let x = Gensym.gensym "x" in
        let i = Gensym.gensym "idx" in
        let res = Gensym.gensym "res" in
        let fsm = 
          access [TPtr;TConst TInt] ty_elem
            CamlComputedField [Atom.Var x;Atom.Var i]
        in
        [],VSML.LetIn([(x,translate_type ty),
                         translate_exp env arr; 
                       (i,TConst TInt),translate_exp env idx],
           VSML.LetIn([(res,translate_type ty_elem),fsm],Atom (Atom.Var res)))  
    | ArrayLength e ->
        let ty = array_ty (ty_of e) in
        let x = Gensym.gensym "x" in
        let res = Gensym.gensym "res" in
        let fsm = access ~callbk:(fun x -> Prim(Size_hd,[x])) 
                       [TPtr] ty CamlHeader [Atom.Var x] in
        [],VSML.LetIn([((x,translate_type (TCamlArray ty)),
                          translate_exp env e)],
           VSML.LetIn([(res,TConst TInt),fsm],Atom (Atom.Var res)))  
    | ListHd e -> 
        let ty = list_ty (ty_of e) in
        let x = Gensym.gensym "x" in
        let ts,e' = access [TPtr] ty (CamlField 0) [Atom.Var x] in
        ts,VSML.LetIn([((x,translate_type (TCamlList ty)),
                          translate_exp env e)],
                      e')
    | ListTl e -> 
        let ty = list_ty (ty_of e) in
        let x = Gensym.gensym "x" in
        let ts,e' = access [TPtr] (TCamlList ty) (CamlField 1) [Atom.Var x] in
        ts,VSML.LetIn([((x,translate_type (TCamlList ty)),
                          translate_exp env e)],
                       e')
     | RefAssign {r;e} ->
        let ty_elem = ref_ty (ty_of r) in
        let x = Gensym.gensym "x" in
        let y = Gensym.gensym "y" in
        let ts,e' = assign ty_elem [TPtr] (CamlField 0) [Atom.Var x] (Atom.Var y) in
        ts,VSML.LetIn([(x,translate_type ty),translate_exp env r;
                       (y,translate_type ty_elem),translate_exp env e], 
                      e')
    | ArrayAssign{arr;idx;e} ->
        let ty_elem = array_ty (ty_of arr) in
        let x = Gensym.gensym "x" in
        let y = Gensym.gensym "y" in
        let z = Gensym.gensym "z" in
        let ts,e' = assign ty_elem [TPtr;TConst TInt] CamlComputedField [Atom.Var x;Atom.Var y] (Atom.Var z) in
        ts,VSML.LetIn([(x,translate_type ty),translate_exp env arr;
                       (y,TConst TInt),translate_exp env idx;
                       (z,translate_type ty_elem),translate_exp env e], 
                      e')
      | (ListFoldLeft _ | ArrayFoldLeft _ | ArrayMapBy _) -> 
        assert false (* already expanded *)  
      )
        
let compile_circuit {x;xs;s;ty=t;e} =
  let fsm = translate_exp [] e in
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
  let s' = List.map (fun (x,t) -> (In,x,translate_type t)) xs@s@
           List.map (fun (x,t) -> (Local,x,translate_type t))
             !new_local_signal @ extra
  in
  VSML.{x;s=s';ty=translate_type t;body=fsm}
