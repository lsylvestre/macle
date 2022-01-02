open Ast.TMACLE

let rewrite_map (e:exp) =
  let rec mapper ~default (env:unit) ((desc,ty) as e) =
    match desc with
    | FlatArrayOp(FlatMap(([x],e),[FlatArrayOp(FlatMap((xs,e'),es)),_]))
      when Transparent.transparent e' ->
      (* [map (fun x -> e) (map (fun xs -> e') es))]
         ~> [map (fun xs -> let x = e' in e) es]
      *)
        mapper ~default (env:unit) @@
        (FlatArrayOp(FlatMap((xs,mk_let1 x e' e),es)),ty)
    | FlatArrayOp(FlatReduce((acc,y,e0),init,(FlatArrayOp(FlatMap(([x],ex),[e])),_))) when Transparent.transparent ex ->
      (* [reduce (fun acc y -> e0) init (map (fun x -> e') e))]
         ~> [reduce (fun acc x -> let y = ex in e0) init e]
      *)
        mapper ~default (env:unit) @@
        (FlatArrayOp(FlatReduce((acc,x,mk_let1 y ex e0),init,e)),ty)
    | FlatArrayOp(FlatMap(([x],e),[FlatArrayOp(FlatMake es),_])) ->
      (* [map (fun x -> e) #[|e1;...en|]]
          ~> [#[|let x = e1 in e;... let x = en in e|]]
      *)
        mapper ~default (env:unit) @@
        (FlatArrayOp(FlatMake (List.map (fun ex -> mk_let1 x ex e) es)),ty)
    | FlatArrayOp(FlatReduce((acc,y,e0),init,(FlatArrayOp(FlatMake es),_))) ->
      (* [reduce (fun acc y -> e0) init #[|e1;...en|]]
         ~>  let acc = ... (let acc = (let acc = accu
                                       and y = e1 in e0)
                            and y = e1 in e0)
             and y = en in e0


         (* à tester *)
      *)
        mapper ~default (env:unit) @@
        List.fold_left (fun accu e -> mk_let [(acc,accu);(y,e)] e0) init es
    | _ ->
        default env e
  in
  Ast_mapper.map mapper () e


let rewrite_map_circuit (c : circuit) : circuit =
  let c = {c with e = rewrite_map c.e} in
  Ast_rename.rename_ast c
