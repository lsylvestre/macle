%{
    open Loc
    open Kast
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

%nonassoc IN
%nonassoc SEMICOL
%nonassoc ELSE OTHERWISE
%nonassoc LEFT_ARROW
%left COLONEQ
%right PIPE_PIPE 
%left LAND
%left LT LE GT GE NEQ EQ
%left PLUS MINUS
%left TIMES
%nonassoc NOT UMINUS
%nonassoc ARRAY_LENGTH LIST_HD LIST_TL 
%nonassoc LPAREN RPAREN BANG

%start <Ast.MACLE.circuit list * string> platform_macle

%%

conditionnelle(exp_cond,exp):
| IF a=exp_cond THEN e1=exp ELSE e2=exp { (a,e1,e2) }

case(exp_cond,const,exp):
| CASE a=exp_cond WITH 
  PIPE? handlers=separated_nonempty_list(PIPE,separated_pair(const,RIGHT_ARROW,exp))
  PIPE? OTHERWISE e=exp   { (a,handlers,e) }

constant:
| b=BOOL_LIT               { Atom.Bool b }
| v=std_logic              { Atom.Std_logic v }
| n=INT_LIT                { Atom.Int n }       
| LPAREN RPAREN            { Atom.Unit }
| LBRACKET RBRACKET        { EmptyList }

call_pstate_flat(E):
| q=ident LPAREN es=separated_nonempty_list(COMMA,E) RPAREN { (q,es) }
| q=ident LPAREN RPAREN { (q,[]) }

macle_let_rec:
| LET REC bs=separated_nonempty_list(AND,fun_bindings)
  IN e=mexp { MACLE.LetRec(bs,e) }

macle_let_fun:
| LET b=fun_bindings IN e=mexp { MACLE.LetFun(b,e) }

fun_bindings:
| x=ident LPAREN xs=separated_list(COMMA,located(ident_or_wildcard)) RPAREN 
  EQ e=mexp { ((x,xs),e) }

located(X):
| x=X { (x,$loc) }

/* expressions in Macle */
mexp:
| LPAREN e=mexp RPAREN            { e }
| x=ident                         { mk_loc $loc @@ MACLE.Var x }
| c=constant                      { mk_loc $loc @@ MACLE.Const c }
| p=conditionnelle(mexp,mexp)     { mk_loc $loc @@ MACLE.If p }
| p=case(mexp,constant,mexp)      { mk_loc $loc @@ MACLE.Case p } 
| p=prim(mexp)                    { mk_loc $loc @@ MACLE.Prim p }
| app=call_pstate_flat(mexp)      { mk_loc $loc @@ MACLE.App app }
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
| e1=mexp SEMICOL e2=mexp 
  { let x = Gensym.gensym "ignore" in 
    mk_loc $loc @@ MACLE.Let([((x,$loc),e1)],e2) }

| LIST_FOLD_LEFT LPAREN q=ident COMMA init=mexp COMMA el=mexp RPAREN
  { mk_loc $loc @@ MACLE.CamlPrim(ListFoldLeft(q,init,el)) }

| ARRAY_FOLD_LEFT LPAREN q=ident COMMA init=mexp COMMA earr=mexp RPAREN
  { mk_loc $loc @@ MACLE.CamlPrim(ArrayFoldLeft(q,init,earr)) }

| ARRAY_MAP LPAREN n=INT_LIT COMMA q=ident COMMA earr=mexp RPAREN
  { mk_loc $loc @@ MACLE.CamlPrim(ArrayMapBy(n,q,earr)) }

/*| MATCH e=mexp WITH PIPE? LBRACKET RBRACKET RIGHT_ARROW e1=mexp 
  PIPE x=ident COLCOL xs=ident RIGHT_ARROW e2=mexp
    { mk_match $loc e e1 x xs e2 }
*/
| MATCH e=mexp WITH PIPE? cases=separated_list(PIPE,match_case)
    { mk_loc $loc @@ MACLE.Match(e,cases) }

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
| cs=separated_nonempty_list(SEMI_SEMI,p) s=QUOTE EOF    { (cs,s) }
| cs=separated_nonempty_list(SEMI_SEMI,p) platform_error { assert false }

platform_error:
| error { syntax_error ~msg:"token \";;;;;;;\" expected.\nA program is of the form:\n  [circuit x(x1,... xn) = e1 ;;\n   ...\n   circuit x(x1,... xn) = en\n   ;;[;]^+\n   <OCaml program>.]" $loc }

/* Macle circuits */
mcircuit:
| CIRCUIT x=IDENT LPAREN xs=separated_list(COMMA,located(ident)) RPAREN 
  EQ e=mexp { {x;xs;e;decoration=$loc} }
| CIRCUIT REC x=IDENT LPAREN xs=separated_list(COMMA,located(ident)) RPAREN EQ e=mexp 
  { let q = x in
    let e = MACLE.LetRec([(q,xs),e],
                        (App(q,List.map (fun (x,loc) -> MACLE.Var x,loc) xs),$loc)),$loc in
    {x;xs;e;decoration=$loc} }

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
| RPAREN r=mexp LPAREN COLONEQ e=mexp 
                            { MACLE.RefAssign{r;e} }
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

prim(E): 
| a1=E c=binop a2=E             { Atom.mk_binop c a1 a2 }
| NOT a=E                       { Atom.mk_unop Atom.Not a  }
| MINUS a=E %prec UMINUS        { Atom.mk_unop Atom.Uminus a }
| a=E MOD n=INT_LIT 
  { match n with
    | 2 -> Atom.mk_unop Atom.Mod2 a
    | _ -> syntax_error ~msg:"modulo 2 expected" $loc }
| a=E DIV n=INT_LIT 
  { match n with
    | 2 -> Atom.mk_unop Atom.DivBy2 a
    | _ -> syntax_error ~msg:"division by 2 expected" $loc }

std_logic:
| ZERO { Atom.Zero }
| ONE  { Atom.One }

%inline binop:
| PLUS { Atom.Add }
| MINUS { Atom.Sub }
| TIMES { Atom.Mul }
| LT { Atom.Lt }
| LE { Atom.Le }
| GT { Atom.Gt }
| GE { Atom.Ge }
| EQ { Atom.Eq }
| NEQ { Atom.Neq }
| LAND { Atom.And }
| PIPE_PIPE { Atom.Or }

ident:
| x=IDENT { x }

ident_or_wildcard:
| x=ident { x }
| WILDCARD { Gensym.gensym "wildcard" }
