%{
    open Loc
    open Ast
    open Types
%}

%token EOF 
%token LPAREN RPAREN COMMA PIPE_PIPE EQ SEMICOL
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
%token DOT RIGHT_ARROW ARRAY_LENGTH ARRAY_FOLD_LEFT ARRAY_MAP
%token TYPE OF INT BOOL UNIT
%token RAISE FAILURE INVALID_ARG
%token <string> STRING_LIT

%right PIPE_PIPE 
%left LAND
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
| MATCH e=mexp WITH PIPE? cases=separated_nonempty_list(PIPE,match_case(mexp)) 
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

| e=caml_prim 
    { mk_loc $loc @@ MACLE.CamlPrim e }

| e1=aexp SEMICOL e2=mexp 
    { let x = Gensym.gensym "ignore" in 
      mk_loc $loc @@ MACLE.Let([((x,$loc),e1)],e2) }

| e=aexp 
    { e }

aexp:
| p=prim          { mk_loc $loc @@ p }
| BANG e=sexp     { mk_loc $loc @@ 
                    MACLE.CamlPrim (MACLE.RefAccess e) }
| e=sexp          { e }

sexp:
| x=ident es=exp+ { mk_loc $loc @@ MACLE.App (x,es) }
| RAISE LPAREN exc=exc RPAREN { mk_loc $loc @@ MACLE.Raise exc }
| e=exp { e }

exp:
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
/* | error { syntax_error $loc } */

ident_or_wildcard_option:
| x=ident { Some x }
| WILDCARD { None } 

platform(p): 
| cs=separated_nonempty_list(SEMI_SEMI,p) s=quote        { (cs,s) }

quote:
| s=QUOTE EOF { s }
| error { syntax_error ~msg:"token \";;;;;;;\" expected.\nA program is of the form:\n  [circuit x(x1,... xn) = e1 ;;\n   ...\n   circuit x(x1,... xn) = en\n   ;;[;]^+\n   <OCaml program>.]" $loc }

/* Macle circuits */
mcircuit:
| CIRCUIT x=IDENT xs=arguments
  EQ e=mexp { {x;xs;e;decoration=$loc} }
| CIRCUIT REC x=IDENT xs=arguments EQ e=mexp 
  { let q = x in
    let e = MACLE.LetRec([(q,xs),e],
                        (App(q,List.map (fun (x,loc) -> MACLE.Var x,loc) xs),$loc)),$loc in
    MACLE.{x;xs;e;decoration=$loc} }

arguments:
| xs=located(ident)+ { xs }

platform_macle: 
| typdef* c=platform(mcircuit) { c }

typdef:
| TYPE x=ident EQ cs=separated_nonempty_list(PIPE,constructor_decl) SEMI_SEMI
  { add_code_typ_constr_decl x cs;
    let rec aux i_constant i_param = function
    | [] -> ()
    | (name,[])::cs -> 
        add_constructor name x i_constant [];
        aux (i_constant+1) i_param cs
    | (name,tys)::cs -> 
        add_constructor name x i_param tys;
        aux i_constant (i_param+1) cs
    in aux 0 0 cs }

constructor_decl:
| c=UP_IDENT { (c,[]) }
| c=UP_IDENT OF tys=separated_nonempty_list(TIMES,typ) { (c,tys) }

typ:
| INT  { TMACLE.t_int }
| BOOL { TMACLE.t_bool }
| UNIT { TMACLE.t_unit }
| x=ident { TConstr(x,[]) }

caml_prim:
| r=sexp COLONEQ e=sexp   { MACLE.RefAssign{r;e} }
| arr=exp DOT LPAREN idx=mexp RPAREN  
                          { MACLE.ArrayAccess{arr;idx} }
| arr=exp DOT LPAREN idx=mexp RPAREN LEFT_ARROW e=sexp 
                          { MACLE.ArrayAssign{arr;idx;e} }
| ARRAY_LENGTH e=exp      { MACLE.ArrayLength e }
| ARRAY_FOLD_LEFT q=ident init=exp earr=exp
                          { MACLE.ArrayFoldLeft(q,init,earr) }
| ARRAY_MAP n=INT_LIT q=ident earr=exp
                          { MACLE.ArrayMapBy(n,q,earr) }

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
| NEQ       { Neq }
| PIPE_PIPE { Or }
| LAND      { And }

ident:
| x=IDENT { x }

ident_or_wildcard:
| x=ident { x }
| WILDCARD { Gensym.gensym "wildcard" }
