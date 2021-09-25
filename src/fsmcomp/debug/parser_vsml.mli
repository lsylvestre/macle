
(* The type of tokens. *)

type token = 
  | ZERO
  | WITH
  | WILDCARD
  | UNIT
  | UMINUS
  | TIMES
  | THEN
  | STD_LOGIC
  | SLASH_SLASH
  | SIG
  | SEMI_SEMI
  | SEMICOL
  | RPAREN
  | RIGHT_ARROW
  | RETURN
  | QUOTE of (string)
  | PLUS
  | PIPE_PIPE
  | PIPE
  | PAR
  | OUTPUT
  | OUT
  | OTHERWISE
  | ONE
  | NOT
  | NEQ
  | MOD
  | MINUS
  | LT
  | LPAREN
  | LOCAL
  | LIST_FOLD_LEFT
  | LET
  | LEFT_ARROW
  | LE
  | LAND
  | INT_LIT of (int)
  | INT
  | INPUT
  | IN
  | IF
  | IDENT of (string)
  | GT
  | GE
  | FUN
  | EQ
  | EOF
  | END
  | ELSE
  | DO
  | DIV
  | CONTINUE
  | COMMA
  | COLONEQ
  | COL
  | CIRCUIT
  | CASE
  | CAP_IDENT of (string)
  | BOOL_LIT of (bool)
  | BOOL
  | AUTOMATON
  | ARRAY_MAP
  | ARRAY_FOLD_LEFT
  | ARRAY
  | AND

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val vsml: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Kast.VSML.circuit)
