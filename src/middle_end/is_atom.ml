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
  | If(e1,e2,e3) -> 
      is_atom e1 && is_atom e2 && is_atom e3
  | PacketPrim c ->
      (match c with
       | PkMake(es) ->
           List.for_all is_atom es
       | PkGet(e,idx) ->
           is_atom e && is_atom idx
       | PkSet _
       | ToPacket _
       | OfPacket _
       | PkMap _
       | PkReduce _
       | PkScan _ ->
           false)
  | Macro _ -> 
     assert false
  | _ -> false
