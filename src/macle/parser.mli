
(* The type of tokens. *)

type token = 
  | WITH
  | WILDCARD
  | UP_IDENT of (string)
  | UNIT
  | TYPE
  | TO_PACKET
  | TO
  | TIMES
  | THEN
  | STRING_LIT of (string)
  | SHARP_LBRACKET_PIPE
  | SEMI_SEMI
  | SEMICOL
  | RPAREN
  | RIGHT_ARROW
  | REC
  | RBRACKET
  | RAISE
  | QUOTE of (string)
  | PLUS
  | PK_SCAN
  | PK_REDUCE
  | PK_MAP
  | PK_GET
  | PIPE_RBRACKET
  | PIPE_PIPE
  | PIPE
  | OF_PACKET
  | OF
  | NOT
  | NEQ
  | MOD
  | MINUS
  | MATCH
  | LT
  | LPAREN
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
  | FUN
  | FOR
  | FAILURE
  | EQEQ
  | EQ
  | EOF
  | ELSE
  | DOT
  | DONE
  | DO
  | DIV
  | COMMA
  | COLONEQ
  | COLCOL
  | CIRCUIT
  | BOOL_LIT of (bool)
  | BOOL
  | BANG
  | ARRAY_SCAN
  | ARRAY_REDUCE
  | ARRAY_MAP
  | ARRAY_MAKE
  | ARRAY_LENGTH
  | AND

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val platform_macle: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ast.MACLE.circuit list * string)
