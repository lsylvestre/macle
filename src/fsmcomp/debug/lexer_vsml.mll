{
  open Parser_vsml
  exception Eof
}

let ident = ['a'-'z''A'-'Z'] ['a'-'z''A'-'Z''0'-'9''_''A'-'Z'''']*
let cap_ident = ['A'-'Z'] ['a'-'z''A'-'Z''0'-'9''_''A'-'Z'''']*

rule token = parse
| '('               { LPAREN }
| ')'               { RPAREN }
| ":="              { COLONEQ }
| ','               { COMMA }
| ';'               { SEMICOL }
| '|'               { PIPE }
| "->"              { RIGHT_ARROW }
| "||"              { PIPE_PIPE }
| "automaton"       { AUTOMATON }
| "end"             { END }
| "do"              { DO }
| "let"             { LET }
| "and"             { AND }
| "in"              { IN }
| "if"              { IF }
| "then"            { THEN }
| "else"            { ELSE }
| "return"          { RETURN }
| "true"            { BOOL_LIT true }
| "false"           { BOOL_LIT false }
| (['0'-'9']+) as n { INT_LIT (int_of_string n) }
| "not"             { NOT }
| "+"               { PLUS }
| "-"               { MINUS }
| "*"               { TIMES }
| "<"               { LT }
| "<="              { LE }
| ">"               { GT }
| ">="              { GE }
| "="               { EQ }
| "<>"              { NEQ }
| "&&"              { LAND }
| "'0'"             { ZERO }
| "'1'"             { ONE }
| "int"             { INT }
| "bool"            { BOOL }
| "std_logic"       { STD_LOGIC }
| "unit"            { UNIT }
| [':']             { COL }
| "circuit"         { CIRCUIT }
| "sig"             { SIG }
| "local"           { LOCAL }
| "input"           { INPUT }
| "output"          { OUTPUT }
| "case"            { CASE }
| "with"            { WITH }
| "otherwise"       { OTHERWISE }
| cap_ident as lxm  { CAP_IDENT lxm }
| ident as lxm      { IDENT lxm }
| ['_']             { WILDCARD }
| ['\n' ]           { (Lexing.new_line lexbuf) ; (token lexbuf) }
| [' ' '\t']        { token lexbuf }
| "(*"              { comment lexbuf }
| eof               { EOF }
| _  as lxm         { failwith (Printf.sprintf "Unexpected character: %c"  lxm) }

and comment = parse
| "*)" { token lexbuf }
| _    { comment lexbuf }
