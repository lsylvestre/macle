%{
    open Err
    open Loc
    open Ast
    open Types

  let typdef x cs =
    add_code_typ_constr_decl x cs;
      let rec aux i_constant i_param = function
      | [] -> ()
      | (name,[])::cs ->
          add_constructor name x i_constant [];
          aux (i_constant+1) i_param cs
      | (name,tys)::cs ->
          add_constructor name x i_param tys;
          aux i_constant (i_param+1) cs
      in aux 0 0 cs


  let pk_map loc xs e es = 
    mk_loc loc @@
      MACLE.PacketPrim (PkMap((List.map (fun x -> x,loc) xs,e),es))

  let pk_reduce loc acc x e init e2 = 
    mk_loc loc @@
    MACLE.PacketPrim (PkReduce(((acc,loc),(x,loc),e),init,e2))

  let pk_scan loc acc x e init e2 = 
    mk_loc loc @@
    MACLE.PacketPrim (PkScan(((acc,loc),(x,loc),e),init,e2))

%}

%token EOF
%token LPAREN RPAREN COMMA PIPE_PIPE EQ EQEQ SEMICOL
%token PIPE LEFT_ARROW
%token LET REC AND IN IF THEN ELSE
%token MATCH WITH WILDCARD
%token <string> IDENT UP_IDENT
%token <bool> BOOL_LIT
%token <int> INT_LIT
%token PLUS MINUS TIMES LT LE GT GE NEQ LAND NOT MOD DIV
%token CIRCUIT
%token <string> QUOTE
%token SEMI_SEMI
%token BANG COLONEQ
%token LBRACKET RBRACKET COLCOL
%token DOT RIGHT_ARROW ARRAY_LENGTH ARRAY_REDUCE ARRAY_MAP ARRAY_SCAN 
%token ARRAY_MAKE PK_GET TO_PACKET OF_PACKET
%token TYPE OF INT BOOL UNIT
%token RAISE FAILURE INVALID_ARG
%token FOR TO DO DONE
%token <string> STRING_LIT

%token SHARP_LBRACKET_PIPE PIPE_RBRACKET

%token FUN PK_MAP PK_REDUCE PK_SCAN

%left LT LE GT GE NEQ EQ
%left PLUS MINUS
%left TIMES

%start <Ast.MACLE.circuit list * string> platform_macle

%%

constant:
| b=BOOL_LIT               { Bool b }
| n=INT_LIT                { Int n }
| LPAREN RPAREN            { Unit }
| LBRACKET RBRACKET        { EmptyList }

fun_bindings:
| x=ident xs=located(ident_or_wildcard)+
  EQ e=mexp { ((x,xs),e) }

located(X):
| x=X { (x,$loc) }

/* Macle expressions */

mexp:
| e1=aexp SEMICOL e2=mexp
    { let x = Gensym.gensym "ignore" in
      mk_loc $loc @@ MACLE.Let([((x,$loc),e1)],e2) }

| e=mmexp { e }

mmexp:
| MATCH e=mexp WITH PIPE? cases=separated_list(PIPE,match_case(mexp))
    { mk_loc $loc @@ MACLE.Match(e,cases) }

| IF a=mexp THEN e1=mexp ELSE e2=mexp
    { mk_loc $loc @@ MACLE.If (a,e1,e2) }

| LET bs=separated_nonempty_list(AND,
      separated_pair(located(ident_or_wildcard),EQ,mexp))
  IN e=mexp
    { mk_loc $loc @@ MACLE.Let(bs,e) }

| LET REC bs=separated_nonempty_list(AND,fun_bindings)
  IN e=mexp
    { mk_loc $loc @@ MACLE.LetRec(bs,e) }

| LET b=fun_bindings IN e=mexp
    { mk_loc $loc @@ MACLE.LetFun(b,e) }

| m=macro { mk_loc $loc @@ MACLE.Macro m }

| FOR i=IDENT EQ e=mexp TO init=mexp DO body=mexp DONE 
    { let loop = Gensym.gensym "loop" in
      let len = Gensym.gensym "len" in
      let open MACLE in
      mk_loc $loc @@ Let([(len,$loc),init],
      mk_loc $loc @@ LetRec([(loop,[(i,$loc)]),
            mk_loc $loc @@ If(mk_loc $loc @@ Binop(Le,mk_loc $loc @@ Var i,mk_loc $loc @@ Var len),
                              mk_loc $loc @@ Let([("_",$loc),body],
                                               mk_loc $loc @@ App(loop,[mk_loc $loc @@
                                                Binop(Add,mk_loc $loc  @@Const (Int 1),mk_loc $loc @@ Var i)])),
                            mk_loc $loc @@ Const Unit)],
            mk_loc $loc @@  App(loop,[e]))) }

| e=aexp { e }

aexp:
| e=caml_prim
    { mk_loc $loc @@ MACLE.CamlPrim e }

| p=prim          { mk_loc $loc @@ p }
| BANG e=sexp     { mk_loc $loc @@
                    MACLE.CamlPrim (MACLE.RefAccess e) }
| e=sexp          { e }


sexp:
| x=ident es=exp+             { mk_loc $loc @@ MACLE.App (x,es) }
| RAISE LPAREN exc=exc RPAREN { mk_loc $loc @@ MACLE.Raise exc }
| e=exp { e }

exp:
| x=ident LBRACKET idx=exp RBRACKET COLONEQ e=sexp
    { mk_loc $loc @@
      MACLE.PacketPrim(PkSet(x,idx,e)) }
| SHARP_LBRACKET_PIPE es=separated_list(SEMICOL,mmexp) PIPE_RBRACKET
    { mk_loc $loc @@
      MACLE.PacketPrim(PkMake es) }
| ARRAY_MAKE n=INT_LIT e=exp 
    { let x = Gensym.gensym "x" in
      mk_loc $loc @@ MACLE.Let([((x,$loc),e)],
                                mk_loc $loc @@ MACLE.PacketPrim(PkMake (List.init n (fun _ -> mk_loc $loc @@ MACLE.Var x))))} 

| TO_PACKET n=INT_LIT e=exp idx=exp
    { mk_loc $loc @@
      MACLE.PacketPrim (ToPacket(e,idx,n)) }

| OF_PACKET n=INT_LIT e1=exp idx=exp e2=exp
    { mk_loc $loc @@
      MACLE.PacketPrim (OfPacket(e1,e2,idx,n)) }

| PK_GET e=exp idx=mexp
    { mk_loc $loc @@
      MACLE.PacketPrim (PkGet(e, idx ))}

| PK_MAP LPAREN FUN xs=ident+ LEFT_ARROW e=mexp RPAREN es=exp+
    { pk_map $loc xs e es }
| PK_REDUCE LPAREN FUN acc=ident x=ident LEFT_ARROW e=mexp RPAREN init=exp e2=exp
    { pk_reduce $loc acc x e init e2 }
| PK_SCAN LPAREN FUN acc=ident x=ident LEFT_ARROW e=mexp RPAREN init=exp e2=exp
    { pk_scan $loc acc x e init e2 }

  /* ==============
     abbreviation : 
     (map f e1 ... en) ~ (map (fun x1 ... xn -> f x1 ... xn) e1 ... en) 
     where x1 ... xn are fresh names 
  */
| PK_MAP f=ident es=exp+
    { let xs = List.map (fun _ -> Gensym.gensym "x") es in
      let e =
        mk_loc $loc @@
        MACLE.App(f,List.map (fun x -> mk_loc $loc @@ MACLE.Var x) xs)
      in
      pk_map $loc xs e es }

  /* ==============
     abbreviation : 
     (reduce f init e) ~ (reduce (fun acc x -> f acc x) init e) 
     where acc and x are fresh names 
  */
| PK_REDUCE f=ident init=exp e2=exp
    { let acc = Gensym.gensym "acc" in
      let x = Gensym.gensym "x" in
      let e =
        mk_loc $loc @@
        MACLE.App(f,List.map (fun x -> mk_loc $loc @@ MACLE.Var x) [acc;x])
      in
      pk_reduce $loc acc x e init e2 }

| PK_REDUCE LPAREN op=binop RPAREN init=exp e2=exp
    { let acc = Gensym.gensym "acc" in
      let x = Gensym.gensym "x" in
      let e =
        mk_loc $loc @@
        MACLE.Binop(op,(mk_loc $loc @@ MACLE.Var acc),
                       (mk_loc $loc @@ MACLE.Var x))
      in
      pk_reduce $loc acc x e init e2 }

  /* ==============
     abbreviation : 
     (scan f init e) ~ (scan (fun acc x -> f acc x) init e) 
     where acc and x are fresh names 
  */
| PK_SCAN f=ident init=exp e2=exp
    { let acc = Gensym.gensym "acc" in
      let x = Gensym.gensym "x" in
      let e =
        mk_loc $loc @@
        MACLE.App(f,List.map (fun x -> mk_loc $loc @@ MACLE.Var x) [acc;x])
      in
      pk_scan $loc acc x e init e2 }


| LPAREN e=mexp RPAREN            { e }
| x=ident                         { mk_loc $loc @@ MACLE.Var x }
| c=constant                      { mk_loc $loc @@ MACLE.Const c }

exc:
| LPAREN e=exc RPAREN      { e }
| FAILURE     s=STRING_LIT { Exception_Failure s }
| INVALID_ARG s=STRING_LIT { Exception_Invalid_arg s }

%inline match_case(exp):
| cstr=constructor RIGHT_ARROW e=exp { let (c,xs) = cstr in (c,xs,e) }

constructor:
| LPAREN c=constructor RPAREN { c }
| x=located(ident_or_wildcard_option) COLCOL y=located(ident_or_wildcard_option)
  { ("::",[x;y]) }
| LBRACKET RBRACKET { ("[]",[]) }
| x=located(ident_or_wildcard_option) COMMA
  y=located(ident_or_wildcard_option) { (",",[x;y]) }
| x=UP_IDENT { (x,[]) }
| x=UP_IDENT LPAREN xs=separated_nonempty_list(COMMA,located(ident_or_wildcard_option)) RPAREN
   { (x,xs) }

ident_or_wildcard_option:
| x=ident { Some x }
| WILDCARD { None }

platform(p):
| cs=separated_nonempty_list(SEMI_SEMI,p) s=quote        { (List.concat cs,s) }

quote:
| s=QUOTE EOF { s }
| error { syntax_error ~msg:"token \";;;;;;;\" expected.\nA program is of the form:\n  [circuit x(x1,... xn) = e1 ;;\n   ...\n   circuit x(x1,... xn) = en\n   ;;[;]^+\n   <OCaml program>.]" $loc }

/* Macle circuits */
mcircuit:
| CIRCUIT cs=separated_nonempty_list(AND,mcircuit_aux)
  { let bs = List.map (fun MACLE.{x;xs;e} -> (x,xs),e) cs in
    List.map (fun (MACLE.{x;xs;decoration=l} as c) -> 
                           MACLE.{c with e = mk_loc l @@ 
                                             LetRec(bs,mk_loc l @@ App(x,List.map (fun (x,l) -> Var x,l) xs))}) cs }
mcircuit_aux:
| x=IDENT xs=arguments
  EQ e=mexp { MACLE.{x;xs;e;decoration=$loc} }

arguments:
| xs=located(ident)+ { xs }

platform_macle:
| typdef* c=platform(mcircuit) { c }

typdef:
| TYPE x=ident EQ cs=separated_nonempty_list(PIPE,constructor_decl) SEMI_SEMI
  { typdef x cs }

constructor_decl:
| c=UP_IDENT { (c,[]) }
| c=UP_IDENT OF tys=prod_typ { (c,tys) }

prod_typ:
| tys=separated_nonempty_list(TIMES,typ)  { tys }

args_typ:
| { [] }
| ty=typ {[ty]}
| LPAREN tys=separated_nonempty_list(COMMA,typ) RPAREN {tys}

typ:
| INT  { TMACLE.t_int }
| BOOL { TMACLE.t_bool }
| UNIT { TMACLE.t_unit }
| tys=args_typ x=ident { TConstr(x,tys) }

caml_prim:
| r=sexp COLONEQ e=sexp   { MACLE.RefAssign{r;e} }
| arr=exp DOT LPAREN idx=mexp RPAREN 
                          { MACLE.ArrayAccess{arr;idx} }
| arr=exp DOT LPAREN idx=mexp RPAREN LEFT_ARROW e=sexp
                          { MACLE.ArrayAssign{arr;idx;e} }
| ARRAY_LENGTH e=exp      { MACLE.ArrayLength e }

macro:
| e1=aexp PIPE_PIPE e2=aexp     { MACLE.LazyOr(e1,e2) }
| e1=aexp LAND e2=aexp          { MACLE.LazyAnd(e1,e2) }

| ARRAY_REDUCE n=skel_depth f=ident init=exp earr=exp
  { MACLE.OCamlArrayReduce(n,f,init,earr) }

| ARRAY_MAP n=skel_depth f=ident src=exp dst=exp
  { MACLE.OCamlArrayMap(n,f,src,dst) }

| ARRAY_SCAN n=skel_depth f=ident init=exp earr=exp eres=exp
  { MACLE.OCamlArrayScan(n,f,init,earr,eres) }


skel_depth:
| n=INT_LIT {n}
| LT n=INT_LIT GT {n}

/*  ******************* Atoms and instructions ******************* */

prim:
| e1=aexp c=binop e2=aexp       { MACLE.Binop(c,e1,e2) }
| NOT e=sexp                    { MACLE.Unop(Not,e)  }
| MINUS e=sexp                  { MACLE.Unop(Uminus,e)  }
| e=sexp MOD n=INT_LIT
  { match n with
    | 2 -> MACLE.Unop(Mod2,e)
    | _ -> syntax_error ~msg:"modulo 2 expected" $loc }
| e=sexp DIV n=INT_LIT
  { match n with
    | 2 -> MACLE.Unop(DivBy2,e)
    | _ -> syntax_error ~msg:"division by 2 expected" $loc }

%inline binop:
| PLUS      { Add }
| MINUS     { Sub }
| TIMES     { Mul }
| LT        { Lt }
| LE        { Le }
| GT        { Gt }
| GE        { Ge }
| EQ        { Eq }
| EQEQ      { Eq }
| NEQ       { Neq }

ident:
| x=IDENT { x }

ident_or_wildcard:
| x=ident { x }
| WILDCARD { Gensym.gensym "wildcard" }
