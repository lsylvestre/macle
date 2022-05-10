open Ast
open TMACLE
open Types

open Gensym

module Tailrec = struct

    exception Not_tailrec of exp

    let tailrec fs e =
      let exception Break in
      let rec aux tailpos (d,_) =
        match d with
        | Let(bs,e) ->
            List.iter (fun (_,e) -> aux false e) bs;
            aux tailpos e
        | App(f,_) ->
            if not tailpos && List.mem f fs then raise Break
        | If(a,e1,e2) ->
            aux tailpos e1;
            aux tailpos e2
        | Match(a,hs) ->
            List.iter (fun (_,_,e) -> aux true e) hs
        | LetRec(bs,e) ->
            List.iter (fun (_,e) -> aux false e) bs;
            aux tailpos e
        | _ -> () in
      try aux true e; true with
      | Break -> false
end

let fv (x:ident) (e:exp) =
  let s = ref [x] in
  let r = ref [] in
  let f (desc,ty) () =
    (match desc with
    | Var x -> if List.mem x !s then () else r := (x,ty)::!r
    | Let(bs,_) ->
        List.iter (fun ((x,_),_) -> s := x::!s) bs
    | LetRec(bs,_) ->
        List.iter (fun ((_,xs),_) ->
           List.iter (fun (x,_) -> s := x::!s) xs
            ) bs
    | _ -> ());
    desc,() in
  Ast_mapper.iter f () e;
  !r

let push xs e =
  StackPrim(Push(xs,e)),ty_of e

let pop xs e =
  StackPrim(LetPop(xs,e)),ty_of e

let save q e =
  StackPrim(Save(q,e)),ty_of e

let restore () =
    StackPrim(Restore)

let push_arg a e =
  StackPrim(Push_arg(a,e))

let add_stack r (e:exp) : exp =
  let rec mapper ~default r ((desc,ty) as e) =
    match desc with
    | If(e1,e2,e3) ->
        assert (Is_atom.is_atom e1);
        If(e1, mapper ~default r e2,
               mapper ~default r e3),ty
    | Match(e1,hs) ->
        assert (Is_atom.is_atom e1);
        Match(e1,List.map (fun (c,xs,e) -> c,xs,mapper ~default r e) hs),ty
    | Let([(x,t),((App(f,_args),_) as app)],e0) when List.mem f r  ->
        (* [_args] are atomes *)
        (* (if List.mem f r then *)
            let k = gensym "k" in
            let e0' = mapper ~default r e0 in
            let ys = fv x e0 in
            LetRec([((k,[]),pop ((x,t)::List.rev ys) e0')],push ys (save k app)),ty
      (*  else Let([(x,t),app],mapper ~default r e0)),ty*)
    | Let(bs,e0) ->
        (* let bs' = List.map (fun (x,e) -> x,mapper ~default r e) bs in *)
        let e0' = mapper ~default r e0 in
        mk_let_cascad bs e0' (* (StackPrim(Push_arg(e0,(StackPrim(Restore),ty_of e0))),ty_of e0) *)
    | LetRec(bs,e) ->
        let fs = List.map (fun ((x,_),_) -> x) bs in
        let r' = fs@r in
        let bs' = List.map (fun (fargs,e) -> fargs,mapper ~default r' e) bs in
        let e' = mapper ~default r' e in
        let k0 = gensym "k0" in
        let x = gensym "x" in
        let b = (k0,[]), pop [(x,ty)] (Var x,ty) in
        (LetRec(b::bs',save k0 @@ e'),ty)
    | App(f,_) ->
        let x = gensym "x" in
        mapper ~default r (Let([(x,ty),e],(Var x,ty)),ty)
    | CamlPrim _ ->
        let x = gensym "x" in
        Let([(x,ty),e],(((push_arg (Var x,ty) (restore(),ty)),ty))),ty
    | _ ->
       if Is_atom.is_atom e then
       ((push_arg e ((restore ()),ty_of e)),ty_of e) else (default r e)
  in
  Ast_mapper.map mapper r e


  let add_stack0 ds e =
     let ty = ty_of e in
     let k0 = gensym "k_stack" in
     let x = gensym "arg" in
     let r' = List.map (fun ((f,_),_) -> f) ds in
     LetRec(((k0,[]),(pop [(x,ty)] (Var x,ty)))::
                      List.map (fun (fxs,e) -> (fxs,add_stack r' e)) ds,
              (StackPrim(Save(k0,e)),ty)),ty

let stack_intro (e:exp) : exp =
  let rec mapper ~default r ((desc,ty) as e) =
    match desc with
   | LetRec(ds,e2) ->
        let fs = List.map (fun ((f,_),_) -> f) ds in
        if not @@ List.for_all (fun (_,e) -> Tailrec.tailrec fs e) ds then (
            Esml2vhdl.allow_stack := true;
            add_stack0 ds (mapper ~default r e2)
        )
        else (LetRec(List.map (fun (fxs,e) -> (fxs, mapper ~default r e)) ds,mapper ~default r e2)),ty
   | _ -> default r e in
   Ast_mapper.map mapper () e


let derecursivation_circuit (c : TMACLE.circuit) : TMACLE.circuit =
  {c with e = stack_intro c.e}
