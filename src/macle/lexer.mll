{
  open Parser
  exception Eof

}

let ident = ['a'-'z'] ['a'-'z''A'-'Z''0'-'9''_''A'-'Z'''']*
let up_ident = ['A'-'Z']['a'-'z''A'-'Z''0'-'9''_''A'-'Z'''']*

rule token = parse
| '('               { LPAREN }
| ')'               { RPAREN }
| ":="              { COLONEQ }
| "::"              { COLCOL }
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
| "match"           { MATCH }
| "then"            { THEN }
| "else"            { ELSE }
| "true"            { BOOL_LIT true }
| "false"           { BOOL_LIT false }
| (['0'-'9']+) as n { INT_LIT (int_of_string n) }
| "not"             { NOT }
| "+"               { PLUS }
| "-"               { MINUS }
| "*"               { TIMES }
| "/"               { DIV }
| "mod"             { MOD }
| "<"               { LT }
| "<="              { LE }
| ">"               { GT }
| ">="              { GE }
| "="               { EQ }
| "=="              { EQEQ }
| "<>"              { NEQ }
| "&&"              { LAND }
| "["               { LBRACKET }
| "]"               { RBRACKET }
| "#[|"             { SHARP_LBRACKET_PIPE }
| "|]"              { PIPE_RBRACKET }
| "array_make"      { ARRAY_MAKE }
| "pk_get"          { PK_GET }
| "fun"             { FUN }
| "map"             { PK_MAP }
| "reduce"          { PK_REDUCE }
| "scan"            { PK_SCAN }
| ['!']             { BANG }
| ['.']             { DOT }
| "<-"              { LEFT_ARROW }
| "array_length"    { ARRAY_LENGTH }
| "circuit"         { CIRCUIT }
| "with"            { WITH }
| "array_map"       { ARRAY_MAP }
| "array_reduce"    { ARRAY_REDUCE }
| "array_scan"      { ARRAY_SCAN }
| "of_packet"       { OF_PACKET }
| "to_packet"       { TO_PACKET }
| "type"            { TYPE }
| "of"              { OF }
| "int"             { INT }
| "bool"            { BOOL }
| "unit"            { UNIT }
| "for"             { FOR }
| "to"              { TO }
| "do"              { DO }
| "done"            { DONE }
| "raise"           { RAISE }
| "Failure"         { FAILURE }
| "Invalid_arg"     { INVALID_ARG }
| ['"'][^'"']*['"'] as s { STRING_LIT s }
| ident as lxm      { IDENT lxm }
| up_ident as lxm   { UP_IDENT lxm }
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
