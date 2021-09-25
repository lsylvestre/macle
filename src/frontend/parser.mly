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
%token <string> IDENT
%token <bool> BOOL_LIT 
%token <int> INT_LIT
%token PLUS MINUS TIMES LT LE GT GE NEQ LAND NOT
%token UMINUS
%token ZERO ONE /* std_logic values */
%token CIRCUIT
%token <string> QUOTE
%token SEMI_SEMI
%token BANG COLONEQ
%token LBRACKET RBRACKET LIST_HD LIST_TL LIST_FOLD_LEFT  
%token DOT RIGHT_ARROW ARRAY_LENGTH ARRAY_FOLD_LEFT ARRAY_MAP

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
| b=BOOL_LIT               { Atom.mk_bool b }
| v=std_logic              { Atom.mk_std_logic v }
| n=INT_LIT                { Atom.mk_int n }       
| LPAREN RPAREN            { Atom.Unit }
| LBRACKET RBRACKET        { EmptyList }

call_pstate_flat(E):
| q=ident LPAREN es=separated_nonempty_list(COMMA,E) RPAREN { (q,es) }

macle_let_rec:
| LET REC bs=separated_nonempty_list(AND,fun_bindings)
  IN e=mexp { MACLE.LetRec(bs,e) }

macle_let_fun:
| LET b=fun_bindings IN e=mexp { MACLE.LetFun(b,e) }

fun_bindings:
| x=ident LPAREN xs=separated_list(COMMA,ident_or_wildcard) RPAREN 
  EQ e=mexp { ((x,xs),e) }

/* expressions in Macle */
mexp:
| LPAREN e=mexp RPAREN            { e }
| x=ident                             { mk_loc $loc @@ MACLE.Var x }
| c=constant                          { mk_loc $loc @@ MACLE.Const c }
| p=conditionnelle(mexp,mexp) { mk_loc $loc @@ MACLE.If p }
| p=case(mexp,constant,mexp)  { mk_loc $loc @@ MACLE.Case p } 
| p=prim(mexp)                    { mk_loc $loc @@ MACLE.Prim p }
| app=call_pstate_flat(mexp)      { mk_loc $loc @@ MACLE.App app }
| LET bs=separated_nonempty_list(AND,
      separated_pair(ident_or_wildcard,EQ,mexp))
  IN e=mexp 
  { mk_loc $loc @@ MACLE.Let(bs,e) }
| LET x=ident_or_wildcard LPAREN 
    xs=separated_list(COMMA,ident) 
    RPAREN EQ e=mexp IN e2=mexp { mk_loc $loc @@ MACLE.LetRec ([((x,xs),e)],e2) } 
| e=macle_let_rec { mk_loc $loc @@ e }
| e=macle_let_fun { mk_loc $loc @@ e}
| e=caml_prim  { mk_loc $loc @@ MACLE.CamlPrim e }
| e1=mexp SEMICOL e2=mexp 
  { let x = Gensym.gensym "ignore" in 
    mk_loc $loc @@ MACLE.Let([(x,e1)],e2) }

| LIST_FOLD_LEFT LPAREN q=ident COMMA init=mexp COMMA el=mexp RPAREN
  { mk_loc $loc @@ MACLE.CamlPrim(ListFoldLeft(q,init,el)) }

| ARRAY_FOLD_LEFT LPAREN q=ident COMMA init=mexp COMMA earr=mexp RPAREN
  { mk_loc $loc @@ MACLE.CamlPrim(ArrayFoldLeft(q,init,earr)) }

| ARRAY_MAP LPAREN n=INT_LIT COMMA q=ident COMMA earr=mexp RPAREN
  { mk_loc $loc @@ MACLE.CamlPrim(ArrayMapBy(n,q,earr)) }

| error { syntax_error $loc }

platform(p): 
| cs=separated_nonempty_list(SEMI_SEMI,p) s=QUOTE EOF    { (cs,s) }
| cs=separated_nonempty_list(SEMI_SEMI,p) platform_error { assert false }

platform_error:
| error { syntax_error ~msg:"token \";;;;;;;\" expected.\nA program is of the form:\n  [circuit x(x1,... xn) = e1 ;;\n   ...\n   circuit x(x1,... xn) = en\n   ;;[;]^+\n   <OCaml program>.]" $loc }

/* Macle circuits */
mcircuit:
| CIRCUIT x=IDENT LPAREN xs=separated_list(COMMA,ident) RPAREN 
  EQ e=mexp { {x;xs;e;loc=$loc} }
| CIRCUIT REC x=IDENT LPAREN xs=separated_list(COMMA,ident) RPAREN EQ e=mexp 
  { let q = x in
    let e = MACLE.LetRec([(q,xs),e],
                        (App(q,List.map (fun x -> mk_loc $loc @@ MACLE.Var x) xs),$loc)),$loc in
    {x;xs;e;loc=$loc} }

platform_macle: c=platform(mcircuit) { c }

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
