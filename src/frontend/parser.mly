%{
    open Loc
    open Ast
    open Types
%}

%token EOF 
%token LPAREN RPAREN COMMA PIPE_PIPE EQ SEMICOL
%token PIPE LEFT_ARROW
%token LET REC AND IN IF THEN ELSE
%token CASE WITH OTHERWISE WILDCARD
%token <string> IDENT UP_IDENT
%token <bool> BOOL_LIT 
%token <int> INT_LIT
%token PLUS MINUS TIMES LT LE GT GE NEQ LAND NOT MOD DIV
%token UMINUS
%token ZERO ONE /* std_logic values */
%token CIRCUIT
%token <string> QUOTE
%token SEMI_SEMI
%token BANG COLONEQ
%token LBRACKET RBRACKET LIST_HD LIST_TL LIST_FOLD_LEFT MATCH COLCOL
%token DOT RIGHT_ARROW ARRAY_LENGTH ARRAY_FOLD_LEFT ARRAY_MAP
%token TYPE OF INT BOOL UNIT
%token RAISE FAILURE INVALID_ARG
%token <string> STRING_LIT

%nonassoc IN
%nonassoc SEMICOL
%nonassoc ELSE
%nonassoc LEFT_ARROW
%left COLONEQ
%right PIPE_PIPE 
%left LAND
%left LT LE GT GE NEQ EQ
%left PLUS MINUS
%left TIMES
%nonassoc NOT UMINUS
%nonassoc ARRAY_LENGTH LIST_HD LIST_TL 
%nonassoc /* LPAREN RPAREN*/ BANG

%start <Ast.MACLE.circuit list * string> platform_macle

%%

conditionnelle(exp_cond,exp):
| IF a=exp_cond THEN e1=exp ELSE e2=exp { (a,e1,e2) }

case(exp_cond,const,exp):
| CASE a=exp_cond WITH 
  PIPE? handlers=separated_nonempty_list(PIPE,separated_pair(const,RIGHT_ARROW,exp))
  PIPE? OTHERWISE e=exp   { (a,handlers,e) }

constant:
| b=BOOL_LIT               { Bool b }
| n=INT_LIT                { Int n }       
| LPAREN RPAREN            { Unit }
| LBRACKET RBRACKET        { EmptyList }

macle_let_rec:
| LET REC bs=separated_nonempty_list(AND,fun_bindings)
  IN e=mexp { MACLE.LetRec(bs,e) }

macle_let_fun:
| LET b=fun_bindings IN e=mexp { MACLE.LetFun(b,e) }

fun_bindings:
| x=ident xs=located(ident_or_wildcard)+
  EQ e=mexp { ((x,xs),e) }

located(X):
| x=X { (x,$loc) }

/* Macle expressions */
mexp:
| x=ident es=exp+ { mk_loc $loc @@ MACLE.App (x,es) }
/*| MINUS a=exp %prec UMINUS      
                  { mk_loc $loc @@ 
                    MACLE.Prim (Atom.mk_unop Atom.Uminus a) }*/
| LIST_FOLD_LEFT q=ident init=exp el=exp
  { mk_loc $loc @@ MACLE.CamlPrim(ListFoldLeft(q,init,el)) }

| ARRAY_FOLD_LEFT q=ident init=exp earr=exp
  { mk_loc $loc @@ MACLE.CamlPrim(ArrayFoldLeft(q,init,earr)) }

| ARRAY_MAP n=INT_LIT q=ident earr=exp
  { mk_loc $loc @@ MACLE.CamlPrim(ArrayMapBy(n,q,earr)) }

| RAISE LPAREN e=exc RPAREN { mk_loc $loc @@ MACLE.Raise e }

| p=prim          { mk_loc $loc @@ p }
| e=exp { e }

exp:
| LPAREN e=mexp RPAREN            { e }
| x=ident                         { mk_loc $loc @@ MACLE.Var x }
| c=constant                      { mk_loc $loc @@ MACLE.Const c }
| p=conditionnelle(mexp,mexp)     { mk_loc $loc @@ MACLE.If p }
| LET bs=separated_nonempty_list(AND,
      separated_pair(located(ident_or_wildcard),EQ,mexp))
  IN e=mexp 
  { mk_loc $loc @@ MACLE.Let(bs,e) }
| LET x=ident_or_wildcard LPAREN 
    xs=separated_list(COMMA,located(ident)) 
    RPAREN EQ e=mexp IN e2=mexp { mk_loc $loc @@ MACLE.LetRec ([((x,xs),e)],e2) } 
| e=macle_let_rec { mk_loc $loc @@ e }
| e=macle_let_fun { mk_loc $loc @@ e}
| e=caml_prim  { mk_loc $loc @@ MACLE.CamlPrim e }
/*| e1=mexp SEMICOL e2=mexp 
  { let x = Gensym.gensym "ignore" in 
    mk_loc $loc @@ MACLE.Let([((x,$loc),e1)],e2) }*/

| MATCH e=mexp WITH PIPE? cases=separated_list(PIPE,match_case)
    { mk_loc $loc @@ MACLE.Match(e,cases) }

exc:
| LPAREN e=exc RPAREN      { e }
| FAILURE     s=STRING_LIT { Exception_Failure s }
| INVALID_ARG s=STRING_LIT { Exception_Invalid_arg s }
match_case:
| cstr=constructor RIGHT_ARROW e=mexp { let (c,xs) = cstr in (c,xs,e) }

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
| error { syntax_error $loc }

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
| LPAREN xs=separated_list(COMMA,located(ident)) RPAREN
| xs=located(ident)+ { xs }
| LPAREN RPAREN { let x = Gensym.gensym "unit" in [ x,$loc ] }

platform_macle: typdef* c=platform(mcircuit) { c }

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
| BANG e=mexp             { MACLE.RefAccess e }
| r=exp COLONEQ e=mexp    { MACLE.RefAssign{r;e} }   /* idéalement, r=mexp */
| x=ident COLONEQ e=mexp  { MACLE.RefAssign{r=(mk_loc $loc @@ MACLE.Var x);e} }
| LPAREN arr=mexp RPAREN DOT 
  LPAREN idx=mexp RPAREN  { MACLE.ArrayAccess{arr;idx} }
| x=ident DOT 
  LPAREN idx=mexp RPAREN  { MACLE.ArrayAccess{arr=(mk_loc $loc @@ MACLE.Var x);idx} }
| LPAREN arr=mexp RPAREN DOT 
  LPAREN idx=mexp RPAREN 
  LEFT_ARROW e=mexp 
                            { MACLE.ArrayAssign{arr;idx;e} }
| x=ident DOT 
  LPAREN idx=mexp RPAREN 
  LEFT_ARROW e=mexp 
                            { MACLE.ArrayAssign{arr=(mk_loc $loc @@ MACLE.Var x);idx;e} }
| ARRAY_LENGTH e=mexp     { MACLE.ArrayLength e }
| LIST_HD e=mexp          { MACLE.ListHd e }
| LIST_TL e=mexp          { MACLE.ListTl e }

/*  ******************* Atoms and instructions ******************* */

prim: 
| e1=mexp c=binop e2=mexp       { MACLE.Binop(c,e1,e2) }
| NOT e=exp                     { MACLE.Unop(Not,e)  }
| e=mexp MOD n=INT_LIT 
  { match n with
    | 2 -> MACLE.Unop(Mod2,e)
    | _ -> syntax_error ~msg:"modulo 2 expected" $loc }
| e=mexp DIV n=INT_LIT 
  { match n with
    | 2 -> MACLE.Unop(DivBy2,e)
    | _ -> syntax_error ~msg:"division by 2 expected" $loc }

std_logic:
| ZERO { Zero }
| ONE  { One }

%inline binop:
| PLUS { Add }
| MINUS { Sub }
| TIMES { Mul }
| LT { Lt }
| LE { Le }
| GT { Gt }
| GE { Ge }
| EQ { Eq }
| NEQ { Neq }

ident:
| x=IDENT { x }

ident_or_wildcard:
| x=ident { x }
| WILDCARD { Gensym.gensym "wildcard" }
