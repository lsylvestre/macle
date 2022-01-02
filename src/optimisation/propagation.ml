open Ast.TMACLE

(* comme [Macle2vsml.is_atom], excepté pour [FlatMake es] *)
let rec propageable (e,_) =
  match e with
  | Var _
  | Const _ -> true
  | Unop (_,e) ->
      propageable e
  | Binop(_,e1,e2) ->
      propageable e1 && propageable e2
  | FlatArrayOp c ->
      (match c with
       | FlatGet{e;idx} ->
           propageable e && propageable idx
       | FlatMake _ ->
           false
       | ArraySub _ ->
           false
       | FlatMap _ | FlatReduce _ -> false
        )
  | _ -> false

  (* on pourrait aussi considérer comme atomes
     les conditionnelles combinatoires *)

module Env = Map.Make(String)

let extend bs env =
    List.fold_right (fun (x,e) env -> Env.add x e env) bs env

let propagation (e:exp) =
  let rec mapper ~default env ((desc,ty) as e) =
    match desc with
    | Var x ->
       (match Env.find_opt x env with
       | None -> desc
       | Some e' -> e'),ty
    | Let(bs,e) ->
      let bs' = Misc.map_snd (mapper ~default env) bs in
      let env_ext,bs = Misc.partition_map
        (function
         | ((x,_),((d,_) as e)) when propageable e -> Misc.Left (x,d)
         | b -> Misc.Right b) bs'
      in
      mk_let bs (mapper ~default (extend env_ext env) e)
  | App(x,es) ->
      let es' = List.map (mapper ~default env) es in
      (match Env.find_opt x env with
       | None -> App(x,es')
       | Some (Var name) -> App(name,List.map (mapper ~default env) es)
       | _ -> assert false),ty
  | _ ->
    default env e
  in
  Ast_mapper.map mapper Env.empty e


let constant_copy_propagation (c : circuit) : circuit =
  {c with e = propagation c.e}
