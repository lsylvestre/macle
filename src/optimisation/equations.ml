open Ast.TMACLE

let rewrite_map (e:exp) =
  let rec mapper ~default (env:unit) ((desc,ty) as e) =
    match desc with
    | FlatArrayOp(Map(([x],e),[FlatArrayOp(Map((xs',e'),es)),_])) when Transparent.transparent e' ->
       mapper ~default (env:unit) @@
       (FlatArrayOp(Map((xs',mk_let1 x e' e),es)),ty)
    | FlatArrayOp(Reduce((acc,y,e0),init,(FlatArrayOp(Map(([x],e'),[ex])),_))) when Transparent.transparent e' ->
        FlatArrayOp(Reduce((acc,x,mk_let1 y e' e0),init,ex)),ty
    | FlatArrayOp(Map(([x],e0),[FlatArrayOp(FlatMake es),_])) ->
        FlatArrayOp(FlatMake (List.map (fun e -> mk_let1 x e0 e) es)),ty
    | FlatArrayOp(Reduce((acc,y,e0),init,(FlatArrayOp(FlatMake es),_))) ->
        List.fold_left (fun accu e -> mk_let [(acc,accu);(y,e)] e0) init es 
    | _ -> 
      default env e
  in
  Ast_mapper.map mapper () e


let rewrite_map_circuit (c : circuit) : circuit = 
  {c with e = rewrite_map c.e}
