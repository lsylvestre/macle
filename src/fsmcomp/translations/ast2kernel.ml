open Ast
open Types 
open TMACLE

let rec is_atom (e,_) = match e with
| Var _ | Const _ -> true
| Prim (p,es) -> List.for_all is_atom es
| _ -> false


let list_names es = 
  List.map (fun e -> Gensym.gensym "x",ty_of e) es

let vars xs =
   List.map (fun (x,ty) -> (Var x,ty)) xs



let rec c_e e = 
  let plug1 e f =
    let e' = c_e e in
    if is_atom e' then f e' else
    let x = Gensym.gensym "x" in
    let ty = ty_of e' in
    mk_let1 (x,ty) e' @@ f (Var x,ty) 
  in
  let plugN es f =
    let es' = List.map c_e es in
    if List.for_all is_atom es' then f es' else
    let xs = List.map (fun e -> Gensym.gensym "x",ty_of e) es' in
    mk_let (List.combine xs es') @@ f @@
    List.map (fun (x,ty) -> Var x,ty) xs
  in
  match e with
  | Const _,_ | Var _,_-> e
  | Prim(p,es),ty ->
      let es' = List.map c_e es in
      if List.for_all is_atom es then (Prim(p,es),ty) else
      let xs = list_names es' in
      mk_let (List.combine xs es') @@ (Prim(p,vars xs),ty)
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
  | Case _,_ -> failwith "todo"
  | Match(e,cases),ty ->
      let cases' = List.map (fun (c,xs,e) -> c,xs,c_e e) cases in
      plug1 e (fun x -> Match(x,cases'),ty)
  | Raise _,_ -> 
      e
  | CamlPrim p,ty ->
        (match p with
        | RefAccess e ->
            plug1 e (fun x -> CamlPrim(RefAccess(x)),ty)
        | ListHd e ->
            plug1 e (fun x -> CamlPrim(ListHd(x)),ty)
        | ListTl e ->
            plug1 e (fun x -> CamlPrim(ListTl(x)),ty)
        | ArrayLength e ->
            plug1 e (fun x -> CamlPrim(ArrayLength(x)),ty)
        | ArrayAccess{arr;idx} ->
            plugN [arr;idx] (function 
              | [arr;idx] -> (CamlPrim(ArrayAccess{arr;idx}),ty)
              | _ -> assert false)
        | RefAssign{r;e} ->
            plugN [r;e] (function 
              | [arr;idx] -> (CamlPrim(RefAssign{r;e}),ty)
              | _ -> assert false)
        | ArrayAssign{arr;idx;e} ->
            plugN [arr;idx;e] (function 
              | [arr;idx;e] -> (CamlPrim(ArrayAssign{arr;idx;e}),ty)
              | _ -> assert false)
        | (ArrayMapBy _ | ArrayFoldLeft _ | ListFoldLeft _) -> 
        assert false (* already expanded *)
  )
let rewrite_circuit c = 
  TMACLE.{c with e = c_e c.e}
