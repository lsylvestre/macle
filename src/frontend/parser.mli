
(* The type of tokens. *)

type token = 
  | ZERO
  | WITH
  | WILDCARD
  | UMINUS
  | TIMES
  | THEN
  | SEMI_SEMI
  | SEMICOL
  | RPAREN
  | RIGHT_ARROW
  | REC
  | RBRACKET
  | QUOTE of (string)
  | PLUS
  | PIPE_PIPE
  | PIPE
  | OTHERWISE
  | ONE
  | NOT
  | NEQ
  | MOD
  | MINUS
  | MATCH
  | LT
  | LPAREN
  | LIST_TL
  | LIST_HD
  | LIST_FOLD_LEFT
  | LET
  | LEFT_ARROW
  | LE
  | LBRACKET
  | LAND
  | INT_LIT of (int)
  | IN
  | IF
  | IDENT of (string)
  | GT
  | GE
  | EQ
  | EOF
  | ELSE
  | DOT
  | DIV
  | COMMA
  | COLONEQ
  | COLCOL
  | CIRCUIT
  | CASE
  | BOOL_LIT of (bool)
  | BANG
  | ARRAY_MAP
  | ARRAY_LENGTH
  | ARRAY_FOLD_LEFT
  | AND

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val platform_macle: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ast.MACLE.circuit list * string)
