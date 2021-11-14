open Kast
open Ast
open Types
open TMACLE
    
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

  let rec is_atom (desc,_ : exp) : bool = 
    match desc with
    | (Var _ | Const _) -> 
        true
    | Prim (_,args) ->
        List.for_all is_atom args  
    | _ -> 
        false

(* [exp_to_atom e] raise [Assert_failure] if [e] is not an atom *)
let rec exp_to_atom (desc,_ : exp) : Atom.atom = 
  match desc with
  | Var x -> 
      Atom.Var x
  | Const n -> 
      Atom.Const n
  | Prim (c,args) -> 
      Prim(c,List.map exp_to_atom args) 
  | _ -> 
      assert false (* not an atom *)

(* produit un code d'accès mémoire à une valeur [v] de type [tv]
   à l'adresse [address+field] retournant [k v]. *)
let access ?(k=fun a -> VSML.Atom a) tv ~address ~field = 
  Esml2vhdl.allow_heap_access := true;
  let q = Gensym.gensym "wait_read" in
  let open VSML in
  let open Atom in
  let t = ((q,[]), (* [address] must not be free in [t] *)
           if_ (eq_ (var_ "avm_rm_waitrequest") std_zero)
               (do_ [("avm_rm_read",std_zero)] 
                  (k @@ Prim(FromCaml (translate_type tv),[var_ "avm_rm_readdata"])))
               (state_ q [])) in
  let e = do_ [("avm_rm_address", compute_adress_ (var_ "caml_heap_base") (var_ address) field);
               ("avm_rm_read", std_one)]
            (state_ q []) in
  ([t],e)

(* produit un code d'accès mémoire à une valeur [v] de type [tv]
  au champ [field] du bloc [e] retournant [k v]. *)
let access_atomic_field ?k e ~field ~tv ~ty =
  let x = Gensym.gensym "x" in
  let ts,e' = access ?k ty ~address:x ~field in
  let e1 = VSML.let_ [((x,translate_type tv),e)] e' in
  ts, e1

let access_computed_field ?k e ~field:e_field ~tv ~ty =
  match e_field with
  | ([],VSML.Atom field) -> 
      access_atomic_field ?k e ~field ~tv ~ty
  | _ ->
    let x = Gensym.gensym "x" in
    let idx = Gensym.gensym "idx" in
    let ts,e' = access ?k ty ~address:x ~field:(Var idx) in
    let e1 = VSML.let_ [((x,translate_type tv),e);
                        ((idx,TConst TInt),e_field)] e' in
    ts, e1

let assign tv ~address ~field a = 
  Esml2vhdl.allow_heap_assign := true;
  let q = Gensym.gensym "wait_write" in
  let open VSML in
  let open Atom in
  let t = ((q,[]), (* variable [address] free in [t] *)
           if_ (eq_ (var_ "avm_wm_waitrequest") std_zero)
               (do_ [("avm_wm_write",Atom.std_zero)] 
                    (Atom (Const Unit)))
               (state_ q [])) in
  let e = do_ [("avm_wm_address", Prim(ComputeAddress,[Var "caml_heap_base"; Var address ; field]));
               ("avm_wm_writedata", Prim(ToCaml (translate_type tv),[a]));
               ("avm_wm_write",Atom.std_one)]
            (state_ q []) in
  ([t],e)

let assign_atomic_field e_addr e ~field ~tv =
  let addr = Gensym.gensym "addr" in
  let x = Gensym.gensym "x" in
  let ts,e' = assign tv ~address:addr ~field (Atom.Var x) in
  let e1 = VSML.let_ [((addr,translate_type TPtr),e_addr); 
                      ((x,translate_type tv),e)] @@ e' in
  ts,e1

let assign_computed_field e_addr e ~field:e_field ~tv =
  match e_field with
  | ([],VSML.Atom field) -> 
      assign_atomic_field e_addr e ~field ~tv
  | _ ->
    let addr = Gensym.gensym "addr" in
    let x = Gensym.gensym "x" in
    let idx = Gensym.gensym "idx" in
    let ts,e' = assign tv ~address:addr ~field:(Var idx) (Atom.Var x) in
    let e1 = VSML.let_ [((addr,translate_type TPtr),e_addr);
                        ((idx,TConst TInt),e_field); 
                        ((x,translate_type tv),e)] @@ e' in
    ts,e1

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
        let x = Gensym.gensym "cond" in
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
        let x = Gensym.gensym "cond" in
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
        access_atomic_field (translate_exp env e) 
          ~field:(Atom.mk_int 0) ~tv:(ty_of e) ~ty
    | ListHd e -> 
        access_atomic_field (translate_exp env e) 
          ~field:(Atom.mk_int 0) ~tv:(ty_of e) ~ty
    | ListTl e -> 
        access_atomic_field (translate_exp env e) 
          ~field:(Atom.mk_int 1) ~tv:(ty_of e) ~ty
    | ArrayLength e ->
        access_atomic_field ~k:(fun e -> Atom (Prim(SizeHeader,[e]))) 
          (translate_exp env e) 
          ~field:(Atom.mk_int (-1)) ~tv:TPtr ~ty:TPtr
    
    | ArrayAccess{arr;idx} ->
               access_computed_field (translate_exp env arr) 
          ~field:(translate_exp env idx) ~tv:(ty_of arr) ~ty
       (* let x = Gensym.gensym "x" in
        let i = Gensym.gensym "idx" in
        let ty = array_ty @@ ty_of arr in
        let ts,e' = access ty ~address:x ~field:(Atom.var_ i) in
        ts,VSML.LetIn([(x,translate_type (ty_of arr)), translate_exp env arr; 
                       (i,TConst TInt), translate_exp env idx], 
                       e') *)
    | RefAssign {r;e} ->
        assign_atomic_field (translate_exp env r) (translate_exp env e) 
           ~field:(Atom.mk_int 0) ~tv:(ref_ty @@ ty_of r)
    | ArrayAssign{arr;idx;e} ->
        assign_computed_field (translate_exp env arr) (translate_exp env e) 
           ~field:(translate_exp env idx) ~tv:(array_ty @@ ty_of arr)
    | ( ArrayMapBy _ | ArrayFoldLeft _ | ListFoldLeft _) -> 
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
  let s' = List.map (fun (x,t) -> (In,x,translate_type t)) xs @ s @ extra
  in
  VSML.{x;s=s';ty=translate_type t;body=fsm}
