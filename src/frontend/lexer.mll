{
  open Parser
  exception Eof

}

let ident = ['a'-'z''A'-'Z'] ['a'-'z''A'-'Z''0'-'9''_''A'-'Z'''']*

rule token = parse
| '('               { LPAREN }
| ')'               { RPAREN }
| ":="              { COLONEQ }
| ','               { COMMA }
| ';'               { SEMICOL }
| '|'               { PIPE }
| "->"              { RIGHT_ARROW }
| "||"              { PIPE_PIPE }
| "let"             { LET }
| "rec"             { REC }
| "and"             { AND }
| "in"              { IN }
| "if"              { IF }
| "then"            { THEN }
| "else"            { ELSE }
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
| "["               { LBRACKET }
| "]"               { RBRACKET }
| ['!']             { BANG }
| ['.']             { DOT }
| "<-"              { LEFT_ARROW }
| "array_length"    { ARRAY_LENGTH }
| "list_hd"         { LIST_HD }
| "list_tl"         { LIST_TL }
| "circuit"         { CIRCUIT }
| "case"            { CASE }
| "with"            { WITH }
| "otherwise"       { OTHERWISE }
| "list_fold_left"  { LIST_FOLD_LEFT }
| "array_fold_left" { ARRAY_FOLD_LEFT }
| "array_map_by"    { ARRAY_MAP }
| ident as lxm      { IDENT lxm }
| ['_']             { WILDCARD }
| ['\n' ]           { (Lexing.new_line lexbuf) ; (token lexbuf) }
| [' ' '\t']        { token lexbuf }
| ";;"              { SEMI_SEMI }
| ";;"(';'+)
  (( _ *) as lxm) eof { QUOTE(lxm) }
| "(*"              { comment lexbuf }
| eof               { EOF }
| _  as lxm         { failwith (Printf.sprintf "Unexpected character: %c"  lxm) }

and comment = parse
| "*)" { token lexbuf }
| _    { comment lexbuf }
