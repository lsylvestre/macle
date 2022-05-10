open Ast
open Types
open TMACLE

open Is_atom  (* todo: juste open *)

let list_names es =
  List.map (fun e -> Gensym.gensym "x",ty_of e) es

let vars xs =
   List.map (fun (x,ty) -> (Var x,ty)) xs



let translate_pk_map xs e es ty =
     assert (List.for_all is_atom es);
     let n = packet_size ty in
     let es' = List.init n (fun i ->
        mk_let (List.map2 (fun (x,t) ex ->
                  assert (is_atom ex);
                  (x,t),(PacketPrim(PkGet(ex,mk_int i)),t)) xs es) @@
        e) in
     Ast_rename.rename_exp @@
     ((PacketPrim(PkMake es')),ty)

let translate_pk_reduce acc y e0 init e =
     assert (is_atom init && is_atom e);
     let tye = ty_of e in
     let ty_elem = packet_ty tye in
     let n = packet_size tye in
     let es = List.init n (fun i ->
        PacketPrim(PkGet(e,mk_int i)),ty_elem) in
     Ast_rename.rename_exp @@
     List.fold_left (fun accv e ->
        mk_let [(acc,accv);(y,e)]
        e0) init es

let translate_pk_scan acc y e0 init e ty =
     assert (is_atom init && is_atom e);
     let tye = ty_of e in
     let ty_elem = packet_ty tye in
     let n = packet_size tye in
     let es = List.init n (fun i ->
        PacketPrim(PkGet(e,mk_int i)),ty_elem) in
     let tyacc = ty_of init in
     let xs = List.mapi (fun i _ -> Gensym.gensym ("x"^string_of_int i),tyacc) es in
     
     Ast_rename.rename_exp @@
     let bs = List.combine xs es in
     
     let rec f (lastx:string * _) = function
     | [] -> PacketPrim(PkMake (List.map (fun (x,ty) -> Var x,ty) xs)),ty
     | (x,e)::bs' -> mk_let1 x (mk_let [(acc,(fun (x,ty) -> Var x,ty) lastx);(y,e)] e0) (f x bs') in
     let x = Gensym.gensym "x", tyacc in
     mk_let1 x init (f x bs)


let rec anf e =

  let plugN es f =
    let es' = List.map anf es in
    (* if List.for_all is_atom es' then f es' else *)
    let xs = List.map (fun e -> Gensym.gensym "x",ty_of e) es' in
    (* mk_let_cascad*)
    mk_let (List.combine xs es') @@ f @@
    List.map (fun (x,ty) -> Var x,ty) xs in

  let plug1 e f = plugN [e] @@ function
  | [x] -> f x
  | _ -> assert false in

  let plug2 e1 e2 f = plugN [e1;e2] @@ function
  | [x1;x2] -> f x1 x2
  | _ -> assert false in

  let plug3 e1 e2 e3 f = plugN [e1;e2;e3] @@ function
  | [x1;x2;x3] -> f x1 x2 x3
  | _ -> assert false in

  match e with
  | Const _,_ | Var _,_-> e
  | Unop(p,e),ty ->
     plug1 e (fun x -> (Unop(p,x),ty))
  | Binop(p,e1,e2),ty ->
     plug2 e1 e2 (fun x1 x2 -> (Binop(p,x1,x2),ty))
  | App(f,es),ty ->
      let es' = List.map anf es in
      if List.for_all is_atom es then (App(f,es),ty) else
      let xs = list_names es' in
      mk_let (List.combine xs es') @@ (App(f,vars xs),ty)
  | If(e1,e2,e3),ty ->
      let e2' = anf e2 in
      let e3' = anf e3 in
      plug1 e1 (fun x -> (If(x,e2',e3'),ty))
  | Let(bs,e),ty ->
      let bs' = List.map (fun (xty,e) -> xty,anf e) bs in
      let e' = anf e in
      Let(bs',e'),ty
  | LetFun _,_ ->
      assert false (* already expanded *)
  | LetRec(bs,e),ty ->
      let bs' = List.map (fun (d,e) -> d,anf e) bs in
      let e' = anf e in
      LetRec(bs',e'),ty
  | Match(e,cases),ty ->
      let cases' = List.map (fun (c,xs,e) -> c,xs,anf e) cases in
      plug1 e (fun x -> Match(x,cases'),ty)
  | Raise _,_ ->
      e
  | CamlPrim p,ty ->
        (match p with
        | RefAccess e ->
            plug1 e @@ fun x ->
              CamlPrim(RefAccess(x)),ty
        | ArrayLength e ->
            plug1 e @@ fun x ->
              CamlPrim(ArrayLength(x)),ty
        | ArrayAccess{arr;idx} ->
            plug2 arr idx @@ fun x1 x2 ->
              CamlPrim(ArrayAccess{arr=x1;idx=x2}),ty
        | RefAssign{r;e} ->
            plug2 r e @@ fun x1 x2 ->
              CamlPrim(RefAssign{r=x1;e=x2}),ty
        | ArrayAssign{arr;idx;e} ->
            plug3 arr idx e @@ fun x1 x2 x3 ->
              CamlPrim(ArrayAssign{arr=x1;idx=x2;e=x3}),ty
        | Ref e -> 
            plug1 e @@ fun x -> CamlPrim(Ref x),ty
        | ArrayMake{size;e} ->
            plug2 size e @@ fun x1 x2 -> CamlPrim(ArrayMake{size=x1;e=x2}),ty)
  | PacketPrim c,ty ->
      (match c with
       | PkMake es ->
           plugN es @@ fun xs ->
             PacketPrim (PkMake xs),ty
       | PkGet(e,idx) ->
            plug2 e idx @@ fun x i ->
             PacketPrim (PkGet(x,i)),ty
       | PkSet(x,idx,e) ->
            plug2 idx e @@ fun i v ->
             PacketPrim (PkSet(x,i,v)),ty
       | ToPacket(e,idx,n) ->
            plug2 e idx @@ fun x i ->
             PacketPrim (ToPacket(x,i,n)),ty
       | OfPacket(e1,e2,idx,n) ->
            plug3 e1 e2 idx @@ fun x1 x2 i ->
             PacketPrim (OfPacket(x1,x2,i,n)),ty
       | PkMap((xs,e),es) ->
            plugN es @@ fun ys ->
              anf @@
              translate_pk_map xs e ys ty
       | PkReduce((acc,y,e0),init,e) ->
            plug2 init e @@ fun x1 x2 ->
              anf @@
              translate_pk_reduce acc y (anf e0) x1 x2
       | PkScan((acc,y,e0),init,e) ->
            plug2 init e @@ fun x1 x2 ->
              anf @@
              translate_pk_scan acc y (anf e0) x1 x2 ty
        )
  | Macro _,_ ->
      assert false (* already expanded *)
  | (StackPrim _,_) as e -> e
      (* assert false *)

let anf_circuit c =
  TMACLE.{c with e = anf c.e}
