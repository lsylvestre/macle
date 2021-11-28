
(* The type of tokens. *)

type token = 
  | ZERO
  | WITH
  | WILDCARD
  | UP_IDENT of (string)
  | UNIT
  | UMINUS
  | TYPE
  | TIMES
  | THEN
  | STRING_LIT of (string)
  | SEMI_SEMI
  | SEMICOL
  | RPAREN
  | RIGHT_ARROW
  | REC
  | RBRACKET
  | RAISE
  | QUOTE of (string)
  | PLUS
  | PIPE_PIPE
  | PIPE
  | OTHERWISE
  | ONE
  | OF
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
  | INVALID_ARG
  | INT_LIT of (int)
  | INT
  | IN
  | IF
  | IDENT of (string)
  | GT
  | GE
  | FAILURE
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
  | BOOL
  | BANG
  | ARRAY_MAP
  | ARRAY_LENGTH
  | ARRAY_FOLD_LEFT
  | AND

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val platform_macle: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ast.MACLE.circuit list * string)
