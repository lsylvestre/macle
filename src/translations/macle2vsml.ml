open Ast
open Types 
open TMACLE

let rec is_atom (e,_) = 
  match e with
  | Var _ 
  | Const _ -> true
  | Unop (_,e) -> 
      is_atom e
  | Binop(_,e1,e2) -> 
      is_atom e1 && is_atom e2
  | _ -> false


let list_names es = 
  List.map (fun e -> Gensym.gensym "x",ty_of e) es

let vars xs =
   List.map (fun (x,ty) -> (Var x,ty)) xs


let rec c_e e = 

  let plugN es f =
    let es' = List.map c_e es in
    if List.for_all is_atom es' then f es' else
    let xs = List.map (fun e -> Gensym.gensym "x",ty_of e) es' in
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
      let es' = List.map c_e es in
      if List.for_all is_atom es then (App(f,es),ty) else
      let xs = list_names es' in
      mk_let (List.combine xs es') @@ (App(f,vars xs),ty)
  | If(e1,e2,e3),ty ->
      let e2' = c_e e2 in
      let e3' = c_e e3 in
      plug1 e1 (fun x -> (If(x,e2',e3'),ty)) 
  | Let(bs,e),ty -> 
      let bs' = List.map (fun (xty,e) -> xty,c_e e) bs in
      let e' = c_e e in
      Let(bs',e'),ty
  | LetFun _,_ -> 
      assert false (* already expanded *)
  | LetRec(bs,e),ty -> 
      let bs' = List.map (fun (d,e) -> d,c_e e) bs in
      let e' = c_e e in
      LetRec(bs',e'),ty
  | Match(e,cases),ty ->
      let cases' = List.map (fun (c,xs,e) -> c,xs,c_e e) cases in
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
        | (ArrayMapBy _ | ArrayFoldLeft _) -> 
            assert false (* already expanded *)
  )
let macle2vsml c = 
  TMACLE.{c with e = c_e c.e}