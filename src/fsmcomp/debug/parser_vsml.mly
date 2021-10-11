%{
    open Loc
    open Kast
    open Ktypes
%}

%token EOF 
%token LPAREN RPAREN COMMA PIPE_PIPE EQ COLONEQ SEMICOL
%token PIPE LEFT_ARROW AUTOMATON END
%token LET AND IN IF THEN ELSE RETURN RIGHT_ARROW
%token WILDCARD
%token <string> IDENT CAP_IDENT
%token <bool> BOOL_LIT 
%token <int> INT_LIT
%token PLUS MINUS TIMES LT LE GT GE NEQ LAND NOT
%token UMINUS
%token ZERO ONE /* std_logic values */
%token INT BOOL UNIT ARRAY STD_LOGIC COL
%token CIRCUIT
%token DO PAR CONTINUE
%token INPUT OUTPUT
%token <string> QUOTE
%token SIG LOCAL OUT SLASH_SLASH SEMI_SEMI DIV MOD
%token CASE WITH OTHERWISE
%token LIST_FOLD_LEFT FUN ARRAY_FOLD_LEFT ARRAY_MAP

%left COLONEQ
%left PIPE_PIPE
%left LAND
%left LT LE GT GE NEQ EQ
%left PLUS MINUS
%left TIMES
%nonassoc NOT UMINUS
%nonassoc BANG 
%nonassoc ARRAY_LENGTH LIST_HD LIST_TL 

%start <Kast.VSML.circuit> vsml

%%

transition(src,exp):
| x=src RIGHT_ARROW e=exp { (x,e) }

signature_decl:
| d=destination x=ident COL t=ty { (d,x,t) }

destination:
| LOCAL { Local }
| INPUT { In }
| OUTPUT { Out }

signature:
| SIG ds=separated_list(COMMA,signature_decl) END { ds }

product(automaton):
| aa=separated_nonempty_list(SLASH_SLASH,automaton) { aa }

automaton(state,exp):
| LPAREN a=automaton(state,exp) RPAREN { a }
| LET AUTOMATON PIPE? ts=separated_list(PIPE,transition(state,exp)) 
  END IN a=exp 
  { (ts,a) }

conditionnelle(exp_cond,exp):
| IF a=exp_cond THEN e1=exp ELSE e2=exp { (a,e1,e2) }

concur_set(exp):
| x=IDENT COLONEQ e1=exp COMMA e2=exp { (x,e1,e2) }

case(exp_cond,const,exp):
| CASE a=exp_cond WITH 
  PIPE? handlers=separated_nonempty_list(PIPE,separated_pair(const,RIGHT_ARROW,exp))
  PIPE? OTHERWISE e=exp   { (a,handlers,e) }

call_pstate:
| q=state LPAREN args=separated_list(COMMA,atom) RPAREN 
  { (q,args) }

pstate:
| q=state LPAREN 
     parameters=separated_list(COMMA,separated_pair(ident,COL,ty)) 
  RPAREN { (q,parameters) }
/*
upstate:
| q=state LPAREN 
     parameters=separated_list(COMMA,ident) 
  RPAREN { (q,parameters) }
*/

/*
exp_psml:
| LPAREN e=exp_psml RPAREN { e }
| a=atom                           { PSML.Atom a }
| p=conditionnelle(atom,exp_psml)  { PSML.If p }
| p=case(atom,constant,exp_psml)   { PSML.Case p }
| p=concur_set(exp_psml)           { PSML.Set p }
| q=call_pstate(exp_psml)          { PSML.State q }
| CONTINUE a=atom                  { PSML.Continue a }
| a=automaton(pstate,exp_psml)     { PSML.Automaton a }             
| e=exp_psml 
  PIPE_PIPE es=product(exp_psml)   { PSML.Product (e::es) }
*/
exp_vsml:
| LPAREN e=exp_vsml RPAREN         { e }
| a=atom                           { VSML.Atom a }
| p=conditionnelle(atom,exp_vsml)  { VSML.If p }
| p=case(atom,constant,exp_vsml)   { VSML.Case p }
| DO bs=separated_nonempty_list(AND,separated_pair(ident,COLONEQ,atom))
  THEN e=exp_vsml                  { VSML.DoThen(bs,e) }
| app=call_pstate                  { VSML.State app }
| CONTINUE a=atom                 { VSML.Continue a }             
| LET bs=separated_nonempty_list(AND,
      separated_pair(separated_pair(ident,COL,ty),EQ,automaton_vsml))
  IN e=exp_vsml 
  { VSML.LetIn(bs,e) }

automaton_vsml:
| fsm=automaton(pstate,exp_vsml) { fsm }
/*
exp_uvsml:
| LPAREN e=exp_uvsml RPAREN { e }
| a=atom                           { UVSML.Atom a }
| p=conditionnelle(atom,exp_uvsml) { VSML.If p }
| p=case(atom,constant,exp_uvsml)  { VSML.Case p }
| DO bs=separated_nonempty_list(AND,separated_pair(ident,COLONEQ,atom))
  IN e=exp_uvsml                   { UVSML.Do(bs,e) }
| app=call_pstate(exp_uvsml)       { UVSML.State app }
| CONTINUE a=atom                  { UVSML.Continue a }
| a=automaton(upstate,exp_uvsml)   { UVSML.Automaton a }             
| LET bindings=separated_nonempty_list(AND,
      separated_pair(ident,EQ,exp_uvsml))
  IN e=exp_uvsml 
  { let q = Gensym.gensym "tmp" in
    let xs,es = List.split bindings in
    UVSML.Automaton ([((q,xs),e)],
                     UVSML.State (q,es)) }
*/
constant:
| b=BOOL_LIT               { Atom.mk_bool b }
| v=std_logic              { Atom.mk_std_logic v }
| n=INT_LIT                { Atom.mk_int n }       
| LPAREN RPAREN            { Atom.Unit }


/*psml: p=prog(exp_psml) EOF { p }
*/
vsml:
| CIRCUIT x=ident 
  COL s=signature RETURN ty=ty 
  EQ body=automaton_vsml EOF
  { {x;s;ty;body} }
| error {failwith "bim"}
/*  ******************* Atoms and instructions ******************* */

state:
| x=CAP_IDENT { x }

const(E):
| b=BOOL_LIT               { Atom.mk_bool b }
| v=std_logic              { Atom.mk_std_logic v }
| n=INT_LIT                { Atom.mk_int n }       
| LPAREN RPAREN            { Atom.Unit }

prim(E): 
| a1=E c=binop a2=E             { Atom.mk_binop c a1 a2 }
| NOT a=E                       { Atom.mk_unop Atom.Not a  }
| a=E DIV n=INT_LIT             { match n with 
                                  | 2 -> Atom.mk_unop Atom.DivBy2 a
                                  | _ -> failwith "division by ... not supported" }
| a=E MOD n=INT_LIT             { match n with 
                                  | 2 -> Atom.mk_unop Atom.Mod2 a
                                  | _ -> failwith "division by ... not supported" }

| MINUS a=E %prec UMINUS        { Atom.mk_unop Atom.Uminus a }
| LPAREN e=E COL t=ty RPAREN    { Atom.mk_ty_annot e t }

std_logic:
| ZERO { Atom.Zero }
| ONE  { Atom.One }

atom:
| x=ident                { Atom.Var x }
| q=state                { Atom.State q }
| c=const(atom)          { Atom.mk_const c }
| p=prim(atom)           { Atom.mk_prim p }
| LPAREN a=atom RPAREN   { a }

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

ty:
| LPAREN ty=ty RPAREN                  { ty }
| STD_LOGIC                            { TConst TStd_logic }
| INT                                  { TConst TInt }
| BOOL                                 { TConst TBool }
| UNIT                                 { TConst TUnit }

ident:
| x=IDENT { x }
| WILDCARD { Gensym.gensym "wildcard" }
