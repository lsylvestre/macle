module ESML = struct

  type ident = string
  type state = string

  type const = Int of int | Bool  of bool

  type ty = TInt | TBool

  type mode = Input | Output | Local
  type signature = (ident * mode * ty) list
  type circuit = Circuit of ident * signature * product

  and product = A of automaton | P of automaton * product
  and automaton = Automaton of (transition list * instruction)
                | Delay of (transition list * instruction)
  and transition = state * instruction 

  and instruction = 
  | Do of (ident * atom) list * instruction
  | If of atom * instruction * instruction
  | Continue of state

  and atom = 
  | Var of ident
  | Const of const
  | Unop of unop * atom
  | Binop of binop * atom * atom  

  and unop = Minus | Not
  and binop = Add | Sub | Mult | And | Or | Lt
end

open ESML

module Env = Map.Make(String);;

let env_lookup x r = 
  Env.find x r

let env_extend bs r = 
  List.fold_right (fun (x,Const c) r -> Env.add x c r) bs r

let sem_unop op c = 
  match op,c with
  | Not,Bool b -> Bool (not b)
  | Minus,Int n -> Int (- n)

let sem_binop op c1 c2 = 
  match op,c1,c2 with
  | Add,Int a,Int b -> Int(a+b)
  | Sub,Int a,Int b -> Int(a-b)
  | Mult,Int a,Int b -> Int(a*b)
  | Lt,Int a,Int b -> Bool(a<b)

let rec red_atom r0 = function
| Var x -> Const (env_lookup x r0)
| Const c -> Const c
| Unop (op,Const c) -> Const (sem_unop op c)
| Binop (op,Const c1,Const c2) -> 
    Const (sem_binop op c1 c2)
(* ctx *)
| Unop (op,a) -> 
    context_atom r0 a @@ fun v -> Unop (op,v)
| Binop (op,((Const _) as v1),a2)  -> 
    context_atom r0 a2 @@ fun a2' -> Binop (op,v1,a2')
| Binop (op,a1,a2) -> 
    context_atom r0 a1 @@ fun a1' -> Binop (op,a1',a2)
and context_atom r0 a ctx_E =
  let a' = red_atom r0 a in
  ctx_E a'

let rec red_inst r0 (s,r) =
  Printf.printf "red_inst";
  match s,r with
  | If(Const (Bool true),s1,s2),r -> s1,r
  | If(Const (Bool false),s1,s2),r -> s1,r
  | Do(bs,s),r 
      when List.for_all (function (x, Const _) -> true | _ -> false) bs ->
    s,(env_extend bs r)
  | If(a,s1,s2),r ->
      context_inst r0 a r @@ fun a' -> If(a',s1,s2)
      
  | Do(bs,s),r ->
      let rec aux acc bs = 
        match bs with
        | b::bs' -> (match b with
                    | _,Const _ -> aux (b::acc) bs'
                    | x,a -> 
                        context_inst r0 a r @@ fun a' -> 
                          Do(List.rev acc@(x,a')::bs',s))
      in aux [] bs

and context_inst r0 a r ctx_F =
  let a' = red_atom r0 a in
  (ctx_F a'), r

let rec red_aut r0 (aut,r) =
  match aut with
  | Automaton (ts,Continue q) -> 
      let s = List.assoc q ts in 
      Delay (ts,s),r
  | Automaton (ts,s) -> 
      context_aut r0 (s,r) @@ fun s' -> Automaton (ts,s')

and context_aut r0 (s,r) ctx_M =
  let s',r' = red_inst r0 (s,r) in
  ctx_M s', r'


let rec red_prod r0 (p,r) =
  match p with
  | A ((Automaton _) as aut) -> 
     let aut',r' = red_aut r0 (aut,r) in
     r0,A aut',r'
  | P (aut,p) -> 
     let aut',r' = red_aut r0 (aut,r) in
     r0,P (aut,p),r'
  | P(((Delay _) as v),p) -> 
      let _,p',r' = red_prod r0 (p,r) in
      r',P(v,p'),r'
  | p -> let p' =
           let rec aux = function
           | A (Delay a) -> A(Automaton a)
           | P(Delay a,p) -> aux (P(Automaton a,aux p)) in
           aux p in 
          r,p',r


let red_circuit r0 (Circuit (x,sg,p)) =
  if List.for_all (fun (x,_,_) -> Env.mem x r0) sg then
    r0,(p,r0)
  else assert false

let aut_ ts s = Automaton(ts,s) ;;

let if_ a s1 s2 = If (a,s1,s2)
let do_ bs s = Do (bs,s)
let continue_ q = Continue q

let (!) x = Var x
let not_ x = Unop(Not,x)
let lt_ a b = Binop(Lt,a,b)
let sub_ a b = Binop(Sub,a,b)
let mult_ a b = Binop(Mult,a,b)

let int_ n = Const(Int n)
let bool_ b = Const(Bool b)

let initial_env bs = env_extend bs Env.empty 

let sum_square = 
  Circuit("sum_square",["start",Input,TBool;
                        "m",Input,TInt;
                        "rdy",Output,TBool;
                        "result",Output,TInt;
                        "n",Local,TInt;
                        "acc",Local,TInt],
    A(aut_ [("Idle",
                do_ ["rdy",not_ !"start"] @@
                if_ !"start" (continue_ "Idle")
                (do_ ["n",!"m";"acc",int_ 0] (continue_ "S")));
            ("S",if_ (lt_ !"n" (int_ 1))
                     (do_ ["result",!"acc"] (continue_ "Idle"))
                     (do_ ["n",sub_ !"n" (int_ 1);"acc",mult_ !"n" !"n"]
                         (continue_ "S")))] 
            (continue_ "Idle")))

let r0,(p,r) = red_circuit (initial_env ["m",int_ 5;
                                  "acc",int_ 0;
                                  "rdy",bool_ true;
                                  "start",bool_ true;
                                  "result",int_ 0;
                                  "n",int_ 0;
                                  ]) sum_square

let r0,p,r = red_prod r0 (p,r);;
Env.bindings r;;


