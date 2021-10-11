
module MenhirBasics = struct
  
  exception Error
  
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
    | QUOTE of (
# 22 "src/fsmcomp/debug/parser_vsml.mly"
       (string)
# 26 "src/fsmcomp/debug/parser_vsml.ml"
  )
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
    | INT_LIT of (
# 14 "src/fsmcomp/debug/parser_vsml.mly"
       (int)
# 51 "src/fsmcomp/debug/parser_vsml.ml"
  )
    | INT
    | INPUT
    | IN
    | IF
    | IDENT of (
# 12 "src/fsmcomp/debug/parser_vsml.mly"
       (string)
# 60 "src/fsmcomp/debug/parser_vsml.ml"
  )
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
    | CAP_IDENT of (
# 12 "src/fsmcomp/debug/parser_vsml.mly"
       (string)
# 80 "src/fsmcomp/debug/parser_vsml.ml"
  )
    | BOOL_LIT of (
# 13 "src/fsmcomp/debug/parser_vsml.mly"
       (bool)
# 85 "src/fsmcomp/debug/parser_vsml.ml"
  )
    | BOOL
    | AUTOMATON
    | ARRAY_MAP
    | ARRAY_FOLD_LEFT
    | ARRAY
    | AND
  
end

include MenhirBasics

let _eRR =
  MenhirBasics.Error

type _menhir_env = {
  _menhir_lexer: Lexing.lexbuf -> token;
  _menhir_lexbuf: Lexing.lexbuf;
  _menhir_token: token;
  mutable _menhir_error: bool
}

and _menhir_state = 
  | MenhirState166
  | MenhirState156
  | MenhirState154
  | MenhirState152
  | MenhirState148
  | MenhirState145
  | MenhirState143
  | MenhirState139
  | MenhirState137
  | MenhirState129
  | MenhirState124
  | MenhirState122
  | MenhirState120
  | MenhirState114
  | MenhirState113
  | MenhirState111
  | MenhirState109
  | MenhirState108
  | MenhirState106
  | MenhirState105
  | MenhirState103
  | MenhirState102
  | MenhirState100
  | MenhirState99
  | MenhirState94
  | MenhirState92
  | MenhirState90
  | MenhirState88
  | MenhirState86
  | MenhirState84
  | MenhirState82
  | MenhirState80
  | MenhirState78
  | MenhirState76
  | MenhirState74
  | MenhirState67
  | MenhirState57
  | MenhirState56
  | MenhirState55
  | MenhirState52
  | MenhirState48
  | MenhirState46
  | MenhirState41
  | MenhirState39
  | MenhirState36
  | MenhirState34
  | MenhirState32
  | MenhirState31
  | MenhirState29
  | MenhirState19
  | MenhirState16
  | MenhirState14
  | MenhirState12
  | MenhirState7
  | MenhirState2

# 1 "src/fsmcomp/debug/parser_vsml.mly"
  
    open Loc
    open Kast
    open Ktypes

# 171 "src/fsmcomp/debug/parser_vsml.ml"

let rec _menhir_goto_separated_nonempty_list_AND_separated_pair_separated_pair_ident_COL_ty__EQ_automaton_vsml__ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_separated_nonempty_list_AND_separated_pair_separated_pair_ident_COL_ty__EQ_automaton_vsml__ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState100 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv661 * _menhir_state) * _menhir_state * 'tv_separated_nonempty_list_AND_separated_pair_separated_pair_ident_COL_ty__EQ_automaton_vsml__) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | IN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv657 * _menhir_state) * _menhir_state * 'tv_separated_nonempty_list_AND_separated_pair_separated_pair_ident_COL_ty__EQ_automaton_vsml__) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | BOOL_LIT _v ->
                _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _v
            | CAP_IDENT _v ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _v
            | CASE ->
                _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState102
            | CONTINUE ->
                _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState102
            | DO ->
                _menhir_run106 _menhir_env (Obj.magic _menhir_stack) MenhirState102
            | IDENT _v ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _v
            | IF ->
                _menhir_run103 _menhir_env (Obj.magic _menhir_stack) MenhirState102
            | INT_LIT _v ->
                _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _v
            | LET ->
                _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState102
            | LPAREN ->
                _menhir_run99 _menhir_env (Obj.magic _menhir_stack) MenhirState102
            | MINUS ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState102
            | NOT ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState102
            | ONE ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState102
            | WILDCARD ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState102
            | ZERO ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState102
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState102) : 'freshtv658)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv659 * _menhir_state) * _menhir_state * 'tv_separated_nonempty_list_AND_separated_pair_separated_pair_ident_COL_ty__EQ_automaton_vsml__) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv660)) : 'freshtv662)
    | MenhirState156 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((('freshtv665 * _menhir_state * 'tv_ident)) * _menhir_state * 'tv_ty)) * _menhir_state * 'tv_automaton_vsml)) * _menhir_state * 'tv_separated_nonempty_list_AND_separated_pair_separated_pair_ident_COL_ty__EQ_automaton_vsml__) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((('freshtv663 * _menhir_state * 'tv_ident)) * _menhir_state * 'tv_ty)) * _menhir_state * 'tv_automaton_vsml)) * _menhir_state * 'tv_separated_nonempty_list_AND_separated_pair_separated_pair_ident_COL_ty__EQ_automaton_vsml__) = Obj.magic _menhir_stack in
        ((let ((((_menhir_stack, _menhir_s, (x : 'tv_ident)), _, (y_inlined1 : 'tv_ty)), _, (y : 'tv_automaton_vsml)), _, (xs : 'tv_separated_nonempty_list_AND_separated_pair_separated_pair_ident_COL_ty__EQ_automaton_vsml__)) = _menhir_stack in
        let _v : 'tv_separated_nonempty_list_AND_separated_pair_separated_pair_ident_COL_ty__EQ_automaton_vsml__ = let x =
          let x =
            let y = y_inlined1 in
            
# 175 "<standard.mly>"
    ( (x, y) )
# 242 "src/fsmcomp/debug/parser_vsml.ml"
            
          in
          
# 175 "<standard.mly>"
    ( (x, y) )
# 248 "src/fsmcomp/debug/parser_vsml.ml"
          
        in
        
# 243 "<standard.mly>"
    ( x :: xs )
# 254 "src/fsmcomp/debug/parser_vsml.ml"
         in
        _menhir_goto_separated_nonempty_list_AND_separated_pair_separated_pair_ident_COL_ty__EQ_automaton_vsml__ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv664)) : 'freshtv666)
    | _ ->
        _menhir_fail ()

and _menhir_goto_loption_separated_nonempty_list_PIPE_transition_pstate_exp_vsml___ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_loption_separated_nonempty_list_PIPE_transition_pstate_exp_vsml___ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ((('freshtv655 * _menhir_state)) * _menhir_state * 'tv_option_PIPE_) * _menhir_state * 'tv_loption_separated_nonempty_list_PIPE_transition_pstate_exp_vsml___) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | END ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv651 * _menhir_state)) * _menhir_state * 'tv_option_PIPE_) * _menhir_state * 'tv_loption_separated_nonempty_list_PIPE_transition_pstate_exp_vsml___) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | IN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv647 * _menhir_state)) * _menhir_state * 'tv_option_PIPE_) * _menhir_state * 'tv_loption_separated_nonempty_list_PIPE_transition_pstate_exp_vsml___)) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | BOOL_LIT _v ->
                _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState166 _v
            | CAP_IDENT _v ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState166 _v
            | CASE ->
                _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState166
            | CONTINUE ->
                _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState166
            | DO ->
                _menhir_run106 _menhir_env (Obj.magic _menhir_stack) MenhirState166
            | IDENT _v ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState166 _v
            | IF ->
                _menhir_run103 _menhir_env (Obj.magic _menhir_stack) MenhirState166
            | INT_LIT _v ->
                _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState166 _v
            | LET ->
                _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState166
            | LPAREN ->
                _menhir_run99 _menhir_env (Obj.magic _menhir_stack) MenhirState166
            | MINUS ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState166
            | NOT ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState166
            | ONE ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState166
            | WILDCARD ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState166
            | ZERO ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState166
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState166) : 'freshtv648)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv649 * _menhir_state)) * _menhir_state * 'tv_option_PIPE_) * _menhir_state * 'tv_loption_separated_nonempty_list_PIPE_transition_pstate_exp_vsml___)) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv650)) : 'freshtv652)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv653 * _menhir_state)) * _menhir_state * 'tv_option_PIPE_) * _menhir_state * 'tv_loption_separated_nonempty_list_PIPE_transition_pstate_exp_vsml___) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv654)) : 'freshtv656)

and _menhir_goto_constant : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_constant -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv645 * _menhir_state * 'tv_constant) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | RIGHT_ARROW ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv641 * _menhir_state * 'tv_constant) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | BOOL_LIT _v ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState137 _v
        | CAP_IDENT _v ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState137 _v
        | CASE ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState137
        | CONTINUE ->
            _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState137
        | DO ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack) MenhirState137
        | IDENT _v ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState137 _v
        | IF ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack) MenhirState137
        | INT_LIT _v ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState137 _v
        | LET ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState137
        | LPAREN ->
            _menhir_run99 _menhir_env (Obj.magic _menhir_stack) MenhirState137
        | MINUS ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState137
        | NOT ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState137
        | ONE ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState137
        | WILDCARD ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState137
        | ZERO ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState137
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState137) : 'freshtv642)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv643 * _menhir_state * 'tv_constant) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv644)) : 'freshtv646)

and _menhir_goto_option_PIPE_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_option_PIPE_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState34 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv631 * _menhir_state)) * _menhir_state * 'tv_option_PIPE_) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | CAP_IDENT _v ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _v
        | END ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv629) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState36 in
            ((let _v : 'tv_loption_separated_nonempty_list_PIPE_transition_pstate_exp_vsml___ = 
# 142 "<standard.mly>"
    ( [] )
# 404 "src/fsmcomp/debug/parser_vsml.ml"
             in
            _menhir_goto_loption_separated_nonempty_list_PIPE_transition_pstate_exp_vsml___ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv630)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState36) : 'freshtv632)
    | MenhirState113 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv633 * _menhir_state) * _menhir_state * 'tv_atom)) * _menhir_state * 'tv_option_PIPE_) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | BOOL_LIT _v ->
            _menhir_run118 _menhir_env (Obj.magic _menhir_stack) MenhirState114 _v
        | INT_LIT _v ->
            _menhir_run117 _menhir_env (Obj.magic _menhir_stack) MenhirState114 _v
        | LPAREN ->
            _menhir_run115 _menhir_env (Obj.magic _menhir_stack) MenhirState114
        | ONE ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState114
        | ZERO ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState114
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState114) : 'freshtv634)
    | MenhirState120 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv639 * _menhir_state) * _menhir_state * 'tv_atom)) * _menhir_state * 'tv_option_PIPE_) * _menhir_state * 'tv_separated_nonempty_list_PIPE_separated_pair_constant_RIGHT_ARROW_exp_vsml__) * _menhir_state * 'tv_option_PIPE_) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | OTHERWISE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((('freshtv635 * _menhir_state) * _menhir_state * 'tv_atom)) * _menhir_state * 'tv_option_PIPE_) * _menhir_state * 'tv_separated_nonempty_list_PIPE_separated_pair_constant_RIGHT_ARROW_exp_vsml__) * _menhir_state * 'tv_option_PIPE_) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | BOOL_LIT _v ->
                _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState122 _v
            | CAP_IDENT _v ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState122 _v
            | CASE ->
                _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState122
            | CONTINUE ->
                _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState122
            | DO ->
                _menhir_run106 _menhir_env (Obj.magic _menhir_stack) MenhirState122
            | IDENT _v ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState122 _v
            | IF ->
                _menhir_run103 _menhir_env (Obj.magic _menhir_stack) MenhirState122
            | INT_LIT _v ->
                _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState122 _v
            | LET ->
                _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState122
            | LPAREN ->
                _menhir_run99 _menhir_env (Obj.magic _menhir_stack) MenhirState122
            | MINUS ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState122
            | NOT ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState122
            | ONE ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState122
            | WILDCARD ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState122
            | ZERO ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState122
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState122) : 'freshtv636)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((('freshtv637 * _menhir_state) * _menhir_state * 'tv_atom)) * _menhir_state * 'tv_option_PIPE_) * _menhir_state * 'tv_separated_nonempty_list_PIPE_separated_pair_constant_RIGHT_ARROW_exp_vsml__) * _menhir_state * 'tv_option_PIPE_) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv638)) : 'freshtv640)
    | _ ->
        _menhir_fail ()

and _menhir_goto_automaton_pstate_exp_vsml_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_automaton_pstate_exp_vsml_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState31 | MenhirState154 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv619 * _menhir_state * 'tv_automaton_pstate_exp_vsml_) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv617 * _menhir_state * 'tv_automaton_pstate_exp_vsml_) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, (fsm : 'tv_automaton_pstate_exp_vsml_)) = _menhir_stack in
        let _v : 'tv_automaton_vsml = 
# 118 "src/fsmcomp/debug/parser_vsml.mly"
                                 ( fsm )
# 500 "src/fsmcomp/debug/parser_vsml.ml"
         in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv615) = _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_automaton_vsml) = _v in
        ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        match _menhir_s with
        | MenhirState154 ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv605 * _menhir_state * 'tv_ident)) * _menhir_state * 'tv_ty)) * _menhir_state * 'tv_automaton_vsml) = Obj.magic _menhir_stack in
            ((assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | AND ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (((('freshtv599 * _menhir_state * 'tv_ident)) * _menhir_state * 'tv_ty)) * _menhir_state * 'tv_automaton_vsml) = Obj.magic _menhir_stack in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | IDENT _v ->
                    _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState156 _v
                | WILDCARD ->
                    _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState156
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState156) : 'freshtv600)
            | IN ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (((('freshtv601 * _menhir_state * 'tv_ident)) * _menhir_state * 'tv_ty)) * _menhir_state * 'tv_automaton_vsml) = Obj.magic _menhir_stack in
                ((let (((_menhir_stack, _menhir_s, (x : 'tv_ident)), _, (y_inlined1 : 'tv_ty)), _, (y : 'tv_automaton_vsml)) = _menhir_stack in
                let _v : 'tv_separated_nonempty_list_AND_separated_pair_separated_pair_ident_COL_ty__EQ_automaton_vsml__ = let x =
                  let x =
                    let y = y_inlined1 in
                    
# 175 "<standard.mly>"
    ( (x, y) )
# 538 "src/fsmcomp/debug/parser_vsml.ml"
                    
                  in
                  
# 175 "<standard.mly>"
    ( (x, y) )
# 544 "src/fsmcomp/debug/parser_vsml.ml"
                  
                in
                
# 241 "<standard.mly>"
    ( [ x ] )
# 550 "src/fsmcomp/debug/parser_vsml.ml"
                 in
                _menhir_goto_separated_nonempty_list_AND_separated_pair_separated_pair_ident_COL_ty__EQ_automaton_vsml__ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv602)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (((('freshtv603 * _menhir_state * 'tv_ident)) * _menhir_state * 'tv_ty)) * _menhir_state * 'tv_automaton_vsml) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv604)) : 'freshtv606)
        | MenhirState31 ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((((('freshtv613) * _menhir_state * 'tv_ident)) * 'tv_signature)) * _menhir_state * 'tv_ty)) * _menhir_state * 'tv_automaton_vsml) = Obj.magic _menhir_stack in
            ((assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | EOF ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ((((((('freshtv609) * _menhir_state * 'tv_ident)) * 'tv_signature)) * _menhir_state * 'tv_ty)) * _menhir_state * 'tv_automaton_vsml) = Obj.magic _menhir_stack in
                ((let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ((((((('freshtv607) * _menhir_state * 'tv_ident)) * 'tv_signature)) * _menhir_state * 'tv_ty)) * _menhir_state * 'tv_automaton_vsml) = Obj.magic _menhir_stack in
                ((let ((((_menhir_stack, _, (x : 'tv_ident)), (s : 'tv_signature)), _, (ty : 'tv_ty)), _, (body : 'tv_automaton_vsml)) = _menhir_stack in
                let _v : (
# 37 "src/fsmcomp/debug/parser_vsml.mly"
       (Kast.VSML.circuit)
# 575 "src/fsmcomp/debug/parser_vsml.ml"
                ) = 
# 151 "src/fsmcomp/debug/parser_vsml.mly"
  ( {x;s;ty;body} )
# 579 "src/fsmcomp/debug/parser_vsml.ml"
                 in
                _menhir_goto_vsml _menhir_env _menhir_stack _v) : 'freshtv608)) : 'freshtv610)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ((((((('freshtv611) * _menhir_state * 'tv_ident)) * 'tv_signature)) * _menhir_state * 'tv_ty)) * _menhir_state * 'tv_automaton_vsml) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv612)) : 'freshtv614)
        | _ ->
            _menhir_fail ()) : 'freshtv616)) : 'freshtv618)) : 'freshtv620)
    | MenhirState32 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv627 * _menhir_state) * _menhir_state * 'tv_automaton_pstate_exp_vsml_) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv623 * _menhir_state) * _menhir_state * 'tv_automaton_pstate_exp_vsml_) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv621 * _menhir_state) * _menhir_state * 'tv_automaton_pstate_exp_vsml_) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _, (a : 'tv_automaton_pstate_exp_vsml_)) = _menhir_stack in
            let _v : 'tv_automaton_pstate_exp_vsml_ = 
# 59 "src/fsmcomp/debug/parser_vsml.mly"
                                       ( a )
# 607 "src/fsmcomp/debug/parser_vsml.ml"
             in
            _menhir_goto_automaton_pstate_exp_vsml_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv622)) : 'freshtv624)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv625 * _menhir_state) * _menhir_state * 'tv_automaton_pstate_exp_vsml_) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv626)) : 'freshtv628)
    | _ ->
        _menhir_fail ()

and _menhir_goto_separated_nonempty_list_PIPE_transition_pstate_exp_vsml__ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_separated_nonempty_list_PIPE_transition_pstate_exp_vsml__ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState39 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv593 * _menhir_state * 'tv_transition_pstate_exp_vsml_)) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_separated_nonempty_list_PIPE_transition_pstate_exp_vsml__) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv591 * _menhir_state * 'tv_transition_pstate_exp_vsml_)) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let ((xs : 'tv_separated_nonempty_list_PIPE_transition_pstate_exp_vsml__) : 'tv_separated_nonempty_list_PIPE_transition_pstate_exp_vsml__) = _v in
        ((let (_menhir_stack, _menhir_s, (x : 'tv_transition_pstate_exp_vsml_)) = _menhir_stack in
        let _v : 'tv_separated_nonempty_list_PIPE_transition_pstate_exp_vsml__ = 
# 243 "<standard.mly>"
    ( x :: xs )
# 636 "src/fsmcomp/debug/parser_vsml.ml"
         in
        _menhir_goto_separated_nonempty_list_PIPE_transition_pstate_exp_vsml__ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv592)) : 'freshtv594)
    | MenhirState36 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv597) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_separated_nonempty_list_PIPE_transition_pstate_exp_vsml__) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv595) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let ((x : 'tv_separated_nonempty_list_PIPE_transition_pstate_exp_vsml__) : 'tv_separated_nonempty_list_PIPE_transition_pstate_exp_vsml__) = _v in
        ((let _v : 'tv_loption_separated_nonempty_list_PIPE_transition_pstate_exp_vsml___ = 
# 144 "<standard.mly>"
    ( x )
# 651 "src/fsmcomp/debug/parser_vsml.ml"
         in
        _menhir_goto_loption_separated_nonempty_list_PIPE_transition_pstate_exp_vsml___ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv596)) : 'freshtv598)
    | _ ->
        _menhir_fail ()

and _menhir_goto_separated_nonempty_list_PIPE_separated_pair_constant_RIGHT_ARROW_exp_vsml__ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_separated_nonempty_list_PIPE_separated_pair_constant_RIGHT_ARROW_exp_vsml__ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState114 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv585 * _menhir_state) * _menhir_state * 'tv_atom)) * _menhir_state * 'tv_option_PIPE_) * _menhir_state * 'tv_separated_nonempty_list_PIPE_separated_pair_constant_RIGHT_ARROW_exp_vsml__) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | PIPE ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState120
        | OTHERWISE ->
            _menhir_reduce41 _menhir_env (Obj.magic _menhir_stack) MenhirState120
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState120) : 'freshtv586)
    | MenhirState139 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv589 * _menhir_state * 'tv_constant)) * _menhir_state * 'tv_exp_vsml)) * _menhir_state * 'tv_separated_nonempty_list_PIPE_separated_pair_constant_RIGHT_ARROW_exp_vsml__) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv587 * _menhir_state * 'tv_constant)) * _menhir_state * 'tv_exp_vsml)) * _menhir_state * 'tv_separated_nonempty_list_PIPE_separated_pair_constant_RIGHT_ARROW_exp_vsml__) = Obj.magic _menhir_stack in
        ((let (((_menhir_stack, _menhir_s, (x : 'tv_constant)), _, (y : 'tv_exp_vsml)), _, (xs : 'tv_separated_nonempty_list_PIPE_separated_pair_constant_RIGHT_ARROW_exp_vsml__)) = _menhir_stack in
        let _v : 'tv_separated_nonempty_list_PIPE_separated_pair_constant_RIGHT_ARROW_exp_vsml__ = let x = 
# 175 "<standard.mly>"
    ( (x, y) )
# 684 "src/fsmcomp/debug/parser_vsml.ml"
         in
        
# 243 "<standard.mly>"
    ( x :: xs )
# 689 "src/fsmcomp/debug/parser_vsml.ml"
         in
        _menhir_goto_separated_nonempty_list_PIPE_separated_pair_constant_RIGHT_ARROW_exp_vsml__ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv588)) : 'freshtv590)
    | _ ->
        _menhir_fail ()

and _menhir_run115 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | RPAREN ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv581 * _menhir_state) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv579 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        let _v : 'tv_constant = 
# 142 "src/fsmcomp/debug/parser_vsml.mly"
                           ( Atom.Unit )
# 711 "src/fsmcomp/debug/parser_vsml.ml"
         in
        _menhir_goto_constant _menhir_env _menhir_stack _menhir_s _v) : 'freshtv580)) : 'freshtv582)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv583 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv584)

and _menhir_run117 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 14 "src/fsmcomp/debug/parser_vsml.mly"
       (int)
# 725 "src/fsmcomp/debug/parser_vsml.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv577) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((n : (
# 14 "src/fsmcomp/debug/parser_vsml.mly"
       (int)
# 735 "src/fsmcomp/debug/parser_vsml.ml"
    )) : (
# 14 "src/fsmcomp/debug/parser_vsml.mly"
       (int)
# 739 "src/fsmcomp/debug/parser_vsml.ml"
    )) = _v in
    ((let _v : 'tv_constant = 
# 141 "src/fsmcomp/debug/parser_vsml.mly"
                           ( Atom.mk_int n )
# 744 "src/fsmcomp/debug/parser_vsml.ml"
     in
    _menhir_goto_constant _menhir_env _menhir_stack _menhir_s _v) : 'freshtv578)

and _menhir_run118 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 13 "src/fsmcomp/debug/parser_vsml.mly"
       (bool)
# 751 "src/fsmcomp/debug/parser_vsml.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv575) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((b : (
# 13 "src/fsmcomp/debug/parser_vsml.mly"
       (bool)
# 761 "src/fsmcomp/debug/parser_vsml.ml"
    )) : (
# 13 "src/fsmcomp/debug/parser_vsml.mly"
       (bool)
# 765 "src/fsmcomp/debug/parser_vsml.ml"
    )) = _v in
    ((let _v : 'tv_constant = 
# 139 "src/fsmcomp/debug/parser_vsml.mly"
                           ( Atom.mk_bool b )
# 770 "src/fsmcomp/debug/parser_vsml.ml"
     in
    _menhir_goto_constant _menhir_env _menhir_stack _menhir_s _v) : 'freshtv576)

and _menhir_goto_separated_nonempty_list_COMMA_separated_pair_ident_COL_ty__ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_separated_nonempty_list_COMMA_separated_pair_ident_COL_ty__ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState41 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv569) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_separated_nonempty_list_COMMA_separated_pair_ident_COL_ty__) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv567) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let ((x : 'tv_separated_nonempty_list_COMMA_separated_pair_ident_COL_ty__) : 'tv_separated_nonempty_list_COMMA_separated_pair_ident_COL_ty__) = _v in
        ((let _v : 'tv_loption_separated_nonempty_list_COMMA_separated_pair_ident_COL_ty___ = 
# 144 "<standard.mly>"
    ( x )
# 789 "src/fsmcomp/debug/parser_vsml.ml"
         in
        _menhir_goto_loption_separated_nonempty_list_COMMA_separated_pair_ident_COL_ty___ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv568)) : 'freshtv570)
    | MenhirState48 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv573 * _menhir_state * 'tv_ident)) * _menhir_state * 'tv_ty)) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_separated_nonempty_list_COMMA_separated_pair_ident_COL_ty__) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv571 * _menhir_state * 'tv_ident)) * _menhir_state * 'tv_ty)) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let ((xs : 'tv_separated_nonempty_list_COMMA_separated_pair_ident_COL_ty__) : 'tv_separated_nonempty_list_COMMA_separated_pair_ident_COL_ty__) = _v in
        ((let ((_menhir_stack, _menhir_s, (x : 'tv_ident)), _, (y : 'tv_ty)) = _menhir_stack in
        let _v : 'tv_separated_nonempty_list_COMMA_separated_pair_ident_COL_ty__ = let x = 
# 175 "<standard.mly>"
    ( (x, y) )
# 805 "src/fsmcomp/debug/parser_vsml.ml"
         in
        
# 243 "<standard.mly>"
    ( x :: xs )
# 810 "src/fsmcomp/debug/parser_vsml.ml"
         in
        _menhir_goto_separated_nonempty_list_COMMA_separated_pair_ident_COL_ty__ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv572)) : 'freshtv574)
    | _ ->
        _menhir_fail ()

and _menhir_run32 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LET ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | LPAREN ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState32

and _menhir_run33 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | AUTOMATON ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv563 * _menhir_state) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | PIPE ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState34
        | CAP_IDENT _ | END ->
            _menhir_reduce41 _menhir_env (Obj.magic _menhir_stack) MenhirState34
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState34) : 'freshtv564)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv565 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv566)

and _menhir_goto_separated_nonempty_list_COMMA_signature_decl_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_separated_nonempty_list_COMMA_signature_decl_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState12 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv557 * _menhir_state * 'tv_signature_decl)) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_separated_nonempty_list_COMMA_signature_decl_) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv555 * _menhir_state * 'tv_signature_decl)) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let ((xs : 'tv_separated_nonempty_list_COMMA_signature_decl_) : 'tv_separated_nonempty_list_COMMA_signature_decl_) = _v in
        ((let (_menhir_stack, _menhir_s, (x : 'tv_signature_decl)) = _menhir_stack in
        let _v : 'tv_separated_nonempty_list_COMMA_signature_decl_ = 
# 243 "<standard.mly>"
    ( x :: xs )
# 875 "src/fsmcomp/debug/parser_vsml.ml"
         in
        _menhir_goto_separated_nonempty_list_COMMA_signature_decl_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv556)) : 'freshtv558)
    | MenhirState7 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv561) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_separated_nonempty_list_COMMA_signature_decl_) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv559) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let ((x : 'tv_separated_nonempty_list_COMMA_signature_decl_) : 'tv_separated_nonempty_list_COMMA_signature_decl_) = _v in
        ((let _v : 'tv_loption_separated_nonempty_list_COMMA_signature_decl__ = 
# 144 "<standard.mly>"
    ( x )
# 890 "src/fsmcomp/debug/parser_vsml.ml"
         in
        _menhir_goto_loption_separated_nonempty_list_COMMA_signature_decl__ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv560)) : 'freshtv562)
    | _ ->
        _menhir_fail ()

and _menhir_goto_std_logic : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_std_logic -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState166 | MenhirState52 | MenhirState99 | MenhirState102 | MenhirState148 | MenhirState105 | MenhirState143 | MenhirState108 | MenhirState137 | MenhirState129 | MenhirState124 | MenhirState122 | MenhirState111 | MenhirState109 | MenhirState103 | MenhirState55 | MenhirState56 | MenhirState92 | MenhirState90 | MenhirState88 | MenhirState86 | MenhirState84 | MenhirState82 | MenhirState80 | MenhirState78 | MenhirState76 | MenhirState74 | MenhirState67 | MenhirState57 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv549) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_std_logic) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv547) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let ((v : 'tv_std_logic) : 'tv_std_logic) = _v in
        ((let _v : 'tv_const_atom_ = 
# 160 "src/fsmcomp/debug/parser_vsml.mly"
                           ( Atom.mk_std_logic v )
# 911 "src/fsmcomp/debug/parser_vsml.ml"
         in
        _menhir_goto_const_atom_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv548)) : 'freshtv550)
    | MenhirState139 | MenhirState114 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv553) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_std_logic) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv551) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let ((v : 'tv_std_logic) : 'tv_std_logic) = _v in
        ((let _v : 'tv_constant = 
# 140 "src/fsmcomp/debug/parser_vsml.mly"
                           ( Atom.mk_std_logic v )
# 926 "src/fsmcomp/debug/parser_vsml.ml"
         in
        _menhir_goto_constant _menhir_env _menhir_stack _menhir_s _v) : 'freshtv552)) : 'freshtv554)
    | _ ->
        _menhir_fail ()

and _menhir_run58 : _menhir_env -> 'ttv_tail * _menhir_state -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv545 * _menhir_state) = Obj.magic _menhir_stack in
    let (_ : _menhir_state) = _menhir_s in
    ((let (_menhir_stack, _menhir_s) = _menhir_stack in
    let _v : 'tv_const_atom_ = 
# 162 "src/fsmcomp/debug/parser_vsml.mly"
                           ( Atom.Unit )
# 942 "src/fsmcomp/debug/parser_vsml.ml"
     in
    _menhir_goto_const_atom_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv546)

and _menhir_goto_loption_separated_nonempty_list_COMMA_atom__ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_loption_separated_nonempty_list_COMMA_atom__ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : (('freshtv543 * _menhir_state * 'tv_state)) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_atom__) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | RPAREN ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv539 * _menhir_state * 'tv_state)) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_atom__) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv537 * _menhir_state * 'tv_state)) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_atom__) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (q : 'tv_state)), _, (xs : 'tv_loption_separated_nonempty_list_COMMA_atom__)) = _menhir_stack in
        let _v : 'tv_call_pstate = let args = 
# 232 "<standard.mly>"
    ( xs )
# 964 "src/fsmcomp/debug/parser_vsml.ml"
         in
        
# 77 "src/fsmcomp/debug/parser_vsml.mly"
  ( (q,args) )
# 969 "src/fsmcomp/debug/parser_vsml.ml"
         in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv535) = _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_call_pstate) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv533) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_call_pstate) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv531) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let ((app : 'tv_call_pstate) : 'tv_call_pstate) = _v in
        ((let _v : 'tv_exp_vsml = 
# 110 "src/fsmcomp/debug/parser_vsml.mly"
                                   ( VSML.State app )
# 986 "src/fsmcomp/debug/parser_vsml.ml"
         in
        _menhir_goto_exp_vsml _menhir_env _menhir_stack _menhir_s _v) : 'freshtv532)) : 'freshtv534)) : 'freshtv536)) : 'freshtv538)) : 'freshtv540)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv541 * _menhir_state * 'tv_state)) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_atom__) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv542)) : 'freshtv544)

and _menhir_reduce2 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_state -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, (q : 'tv_state)) = _menhir_stack in
    let _v : 'tv_atom = 
# 183 "src/fsmcomp/debug/parser_vsml.mly"
                         ( Atom.State q )
# 1003 "src/fsmcomp/debug/parser_vsml.ml"
     in
    _menhir_goto_atom _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_loption_separated_nonempty_list_COMMA_separated_pair_ident_COL_ty___ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_loption_separated_nonempty_list_COMMA_separated_pair_ident_COL_ty___ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : (('freshtv529 * _menhir_state * 'tv_state)) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_separated_pair_ident_COL_ty___) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | RPAREN ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv525 * _menhir_state * 'tv_state)) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_separated_pair_ident_COL_ty___) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv523 * _menhir_state * 'tv_state)) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_separated_pair_ident_COL_ty___) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (q : 'tv_state)), _, (xs : 'tv_loption_separated_nonempty_list_COMMA_separated_pair_ident_COL_ty___)) = _menhir_stack in
        let _v : 'tv_pstate = let parameters = 
# 232 "<standard.mly>"
    ( xs )
# 1025 "src/fsmcomp/debug/parser_vsml.ml"
         in
        
# 82 "src/fsmcomp/debug/parser_vsml.mly"
         ( (q,parameters) )
# 1030 "src/fsmcomp/debug/parser_vsml.ml"
         in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv521) = _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_pstate) = _v in
        ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv519 * _menhir_state * 'tv_pstate) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RIGHT_ARROW ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv515 * _menhir_state * 'tv_pstate) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | BOOL_LIT _v ->
                _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _v
            | CAP_IDENT _v ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _v
            | CASE ->
                _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState52
            | CONTINUE ->
                _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState52
            | DO ->
                _menhir_run106 _menhir_env (Obj.magic _menhir_stack) MenhirState52
            | IDENT _v ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _v
            | IF ->
                _menhir_run103 _menhir_env (Obj.magic _menhir_stack) MenhirState52
            | INT_LIT _v ->
                _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _v
            | LET ->
                _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState52
            | LPAREN ->
                _menhir_run99 _menhir_env (Obj.magic _menhir_stack) MenhirState52
            | MINUS ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState52
            | NOT ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState52
            | ONE ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState52
            | WILDCARD ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState52
            | ZERO ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState52
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState52) : 'freshtv516)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv517 * _menhir_state * 'tv_pstate) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv518)) : 'freshtv520)) : 'freshtv522)) : 'freshtv524)) : 'freshtv526)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv527 * _menhir_state * 'tv_state)) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_separated_pair_ident_COL_ty___) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv528)) : 'freshtv530)

and _menhir_goto_const_atom_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_const_atom_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv513) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_const_atom_) = _v in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv511) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((c : 'tv_const_atom_) : 'tv_const_atom_) = _v in
    ((let _v : 'tv_atom = 
# 184 "src/fsmcomp/debug/parser_vsml.mly"
                         ( Atom.mk_const c )
# 1110 "src/fsmcomp/debug/parser_vsml.ml"
     in
    _menhir_goto_atom _menhir_env _menhir_stack _menhir_s _v) : 'freshtv512)) : 'freshtv514)

and _menhir_goto_separated_nonempty_list_AND_separated_pair_ident_COLONEQ_atom__ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_separated_nonempty_list_AND_separated_pair_ident_COLONEQ_atom__ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState106 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv505 * _menhir_state) * _menhir_state * 'tv_separated_nonempty_list_AND_separated_pair_ident_COLONEQ_atom__) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | THEN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv501 * _menhir_state) * _menhir_state * 'tv_separated_nonempty_list_AND_separated_pair_ident_COLONEQ_atom__) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | BOOL_LIT _v ->
                _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _v
            | CAP_IDENT _v ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _v
            | CASE ->
                _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState108
            | CONTINUE ->
                _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState108
            | DO ->
                _menhir_run106 _menhir_env (Obj.magic _menhir_stack) MenhirState108
            | IDENT _v ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _v
            | IF ->
                _menhir_run103 _menhir_env (Obj.magic _menhir_stack) MenhirState108
            | INT_LIT _v ->
                _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _v
            | LET ->
                _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState108
            | LPAREN ->
                _menhir_run99 _menhir_env (Obj.magic _menhir_stack) MenhirState108
            | MINUS ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState108
            | NOT ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState108
            | ONE ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState108
            | WILDCARD ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState108
            | ZERO ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState108
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState108) : 'freshtv502)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv503 * _menhir_state) * _menhir_state * 'tv_separated_nonempty_list_AND_separated_pair_ident_COLONEQ_atom__) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv504)) : 'freshtv506)
    | MenhirState145 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv509 * _menhir_state * 'tv_ident)) * _menhir_state * 'tv_atom)) * _menhir_state * 'tv_separated_nonempty_list_AND_separated_pair_ident_COLONEQ_atom__) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv507 * _menhir_state * 'tv_ident)) * _menhir_state * 'tv_atom)) * _menhir_state * 'tv_separated_nonempty_list_AND_separated_pair_ident_COLONEQ_atom__) = Obj.magic _menhir_stack in
        ((let (((_menhir_stack, _menhir_s, (x : 'tv_ident)), _, (y : 'tv_atom)), _, (xs : 'tv_separated_nonempty_list_AND_separated_pair_ident_COLONEQ_atom__)) = _menhir_stack in
        let _v : 'tv_separated_nonempty_list_AND_separated_pair_ident_COLONEQ_atom__ = let x = 
# 175 "<standard.mly>"
    ( (x, y) )
# 1180 "src/fsmcomp/debug/parser_vsml.ml"
         in
        
# 243 "<standard.mly>"
    ( x :: xs )
# 1185 "src/fsmcomp/debug/parser_vsml.ml"
         in
        _menhir_goto_separated_nonempty_list_AND_separated_pair_ident_COLONEQ_atom__ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv508)) : 'freshtv510)
    | _ ->
        _menhir_fail ()

and _menhir_goto_separated_nonempty_list_COMMA_atom_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_separated_nonempty_list_COMMA_atom_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState124 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv495) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_separated_nonempty_list_COMMA_atom_) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv493) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let ((x : 'tv_separated_nonempty_list_COMMA_atom_) : 'tv_separated_nonempty_list_COMMA_atom_) = _v in
        ((let _v : 'tv_loption_separated_nonempty_list_COMMA_atom__ = 
# 144 "<standard.mly>"
    ( x )
# 1206 "src/fsmcomp/debug/parser_vsml.ml"
         in
        _menhir_goto_loption_separated_nonempty_list_COMMA_atom__ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv494)) : 'freshtv496)
    | MenhirState129 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv499 * _menhir_state * 'tv_atom)) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_separated_nonempty_list_COMMA_atom_) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv497 * _menhir_state * 'tv_atom)) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let ((xs : 'tv_separated_nonempty_list_COMMA_atom_) : 'tv_separated_nonempty_list_COMMA_atom_) = _v in
        ((let (_menhir_stack, _menhir_s, (x : 'tv_atom)) = _menhir_stack in
        let _v : 'tv_separated_nonempty_list_COMMA_atom_ = 
# 243 "<standard.mly>"
    ( x :: xs )
# 1222 "src/fsmcomp/debug/parser_vsml.ml"
         in
        _menhir_goto_separated_nonempty_list_COMMA_atom_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv498)) : 'freshtv500)
    | _ ->
        _menhir_fail ()

and _menhir_reduce41 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_option_PIPE_ = 
# 114 "<standard.mly>"
    ( None )
# 1233 "src/fsmcomp/debug/parser_vsml.ml"
     in
    _menhir_goto_option_PIPE_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run35 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv491) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let x = () in
    let _v : 'tv_option_PIPE_ = 
# 116 "<standard.mly>"
    ( Some x )
# 1247 "src/fsmcomp/debug/parser_vsml.ml"
     in
    _menhir_goto_option_PIPE_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv492)

and _menhir_goto_exp_vsml : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_exp_vsml -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState122 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((((('freshtv431 * _menhir_state) * _menhir_state * 'tv_atom)) * _menhir_state * 'tv_option_PIPE_) * _menhir_state * 'tv_separated_nonempty_list_PIPE_separated_pair_constant_RIGHT_ARROW_exp_vsml__) * _menhir_state * 'tv_option_PIPE_)) * _menhir_state * 'tv_exp_vsml) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((((('freshtv429 * _menhir_state) * _menhir_state * 'tv_atom)) * _menhir_state * 'tv_option_PIPE_) * _menhir_state * 'tv_separated_nonempty_list_PIPE_separated_pair_constant_RIGHT_ARROW_exp_vsml__) * _menhir_state * 'tv_option_PIPE_)) * _menhir_state * 'tv_exp_vsml) = Obj.magic _menhir_stack in
        ((let ((((((_menhir_stack, _menhir_s), _, (a : 'tv_atom)), _, _), _, (handlers : 'tv_separated_nonempty_list_PIPE_separated_pair_constant_RIGHT_ARROW_exp_vsml__)), _, _), _, (e : 'tv_exp_vsml)) = _menhir_stack in
        let _v : 'tv_case_atom_constant_exp_vsml_ = 
# 73 "src/fsmcomp/debug/parser_vsml.mly"
                          ( (a,handlers,e) )
# 1264 "src/fsmcomp/debug/parser_vsml.ml"
         in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv427) = _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_case_atom_constant_exp_vsml_) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv425) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_case_atom_constant_exp_vsml_) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv423) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let ((p : 'tv_case_atom_constant_exp_vsml_) : 'tv_case_atom_constant_exp_vsml_) = _v in
        ((let _v : 'tv_exp_vsml = 
# 107 "src/fsmcomp/debug/parser_vsml.mly"
                                   ( VSML.Case p )
# 1281 "src/fsmcomp/debug/parser_vsml.ml"
         in
        _menhir_goto_exp_vsml _menhir_env _menhir_stack _menhir_s _v) : 'freshtv424)) : 'freshtv426)) : 'freshtv428)) : 'freshtv430)) : 'freshtv432)
    | MenhirState137 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv439 * _menhir_state * 'tv_constant)) * _menhir_state * 'tv_exp_vsml) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | PIPE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv433 * _menhir_state * 'tv_constant)) * _menhir_state * 'tv_exp_vsml) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | BOOL_LIT _v ->
                _menhir_run118 _menhir_env (Obj.magic _menhir_stack) MenhirState139 _v
            | INT_LIT _v ->
                _menhir_run117 _menhir_env (Obj.magic _menhir_stack) MenhirState139 _v
            | LPAREN ->
                _menhir_run115 _menhir_env (Obj.magic _menhir_stack) MenhirState139
            | ONE ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState139
            | ZERO ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState139
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState139) : 'freshtv434)
        | OTHERWISE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv435 * _menhir_state * 'tv_constant)) * _menhir_state * 'tv_exp_vsml) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (x : 'tv_constant)), _, (y : 'tv_exp_vsml)) = _menhir_stack in
            let _v : 'tv_separated_nonempty_list_PIPE_separated_pair_constant_RIGHT_ARROW_exp_vsml__ = let x = 
# 175 "<standard.mly>"
    ( (x, y) )
# 1317 "src/fsmcomp/debug/parser_vsml.ml"
             in
            
# 241 "<standard.mly>"
    ( [ x ] )
# 1322 "src/fsmcomp/debug/parser_vsml.ml"
             in
            _menhir_goto_separated_nonempty_list_PIPE_separated_pair_constant_RIGHT_ARROW_exp_vsml__ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv436)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv437 * _menhir_state * 'tv_constant)) * _menhir_state * 'tv_exp_vsml) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv438)) : 'freshtv440)
    | MenhirState108 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv443 * _menhir_state) * _menhir_state * 'tv_separated_nonempty_list_AND_separated_pair_ident_COLONEQ_atom__)) * _menhir_state * 'tv_exp_vsml) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv441 * _menhir_state) * _menhir_state * 'tv_separated_nonempty_list_AND_separated_pair_ident_COLONEQ_atom__)) * _menhir_state * 'tv_exp_vsml) = Obj.magic _menhir_stack in
        ((let (((_menhir_stack, _menhir_s), _, (bs : 'tv_separated_nonempty_list_AND_separated_pair_ident_COLONEQ_atom__)), _, (e : 'tv_exp_vsml)) = _menhir_stack in
        let _v : 'tv_exp_vsml = 
# 109 "src/fsmcomp/debug/parser_vsml.mly"
                                   ( VSML.DoThen(bs,e) )
# 1341 "src/fsmcomp/debug/parser_vsml.ml"
         in
        _menhir_goto_exp_vsml _menhir_env _menhir_stack _menhir_s _v) : 'freshtv442)) : 'freshtv444)
    | MenhirState105 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv449 * _menhir_state) * _menhir_state * 'tv_atom)) * _menhir_state * 'tv_exp_vsml) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ELSE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv445 * _menhir_state) * _menhir_state * 'tv_atom)) * _menhir_state * 'tv_exp_vsml) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | BOOL_LIT _v ->
                _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState148 _v
            | CAP_IDENT _v ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState148 _v
            | CASE ->
                _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState148
            | CONTINUE ->
                _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState148
            | DO ->
                _menhir_run106 _menhir_env (Obj.magic _menhir_stack) MenhirState148
            | IDENT _v ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState148 _v
            | IF ->
                _menhir_run103 _menhir_env (Obj.magic _menhir_stack) MenhirState148
            | INT_LIT _v ->
                _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState148 _v
            | LET ->
                _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState148
            | LPAREN ->
                _menhir_run99 _menhir_env (Obj.magic _menhir_stack) MenhirState148
            | MINUS ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState148
            | NOT ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState148
            | ONE ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState148
            | WILDCARD ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState148
            | ZERO ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState148
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState148) : 'freshtv446)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv447 * _menhir_state) * _menhir_state * 'tv_atom)) * _menhir_state * 'tv_exp_vsml) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv448)) : 'freshtv450)
    | MenhirState148 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv459 * _menhir_state) * _menhir_state * 'tv_atom)) * _menhir_state * 'tv_exp_vsml)) * _menhir_state * 'tv_exp_vsml) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv457 * _menhir_state) * _menhir_state * 'tv_atom)) * _menhir_state * 'tv_exp_vsml)) * _menhir_state * 'tv_exp_vsml) = Obj.magic _menhir_stack in
        ((let ((((_menhir_stack, _menhir_s), _, (a : 'tv_atom)), _, (e1 : 'tv_exp_vsml)), _, (e2 : 'tv_exp_vsml)) = _menhir_stack in
        let _v : 'tv_conditionnelle_atom_exp_vsml_ = 
# 65 "src/fsmcomp/debug/parser_vsml.mly"
                                        ( (a,e1,e2) )
# 1406 "src/fsmcomp/debug/parser_vsml.ml"
         in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv455) = _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_conditionnelle_atom_exp_vsml_) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv453) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_conditionnelle_atom_exp_vsml_) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv451) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let ((p : 'tv_conditionnelle_atom_exp_vsml_) : 'tv_conditionnelle_atom_exp_vsml_) = _v in
        ((let _v : 'tv_exp_vsml = 
# 106 "src/fsmcomp/debug/parser_vsml.mly"
                                   ( VSML.If p )
# 1423 "src/fsmcomp/debug/parser_vsml.ml"
         in
        _menhir_goto_exp_vsml _menhir_env _menhir_stack _menhir_s _v) : 'freshtv452)) : 'freshtv454)) : 'freshtv456)) : 'freshtv458)) : 'freshtv460)
    | MenhirState102 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv463 * _menhir_state) * _menhir_state * 'tv_separated_nonempty_list_AND_separated_pair_separated_pair_ident_COL_ty__EQ_automaton_vsml__)) * _menhir_state * 'tv_exp_vsml) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv461 * _menhir_state) * _menhir_state * 'tv_separated_nonempty_list_AND_separated_pair_separated_pair_ident_COL_ty__EQ_automaton_vsml__)) * _menhir_state * 'tv_exp_vsml) = Obj.magic _menhir_stack in
        ((let (((_menhir_stack, _menhir_s), _, (bs : 'tv_separated_nonempty_list_AND_separated_pair_separated_pair_ident_COL_ty__EQ_automaton_vsml__)), _, (e : 'tv_exp_vsml)) = _menhir_stack in
        let _v : 'tv_exp_vsml = 
# 115 "src/fsmcomp/debug/parser_vsml.mly"
  ( VSML.LetIn(bs,e) )
# 1435 "src/fsmcomp/debug/parser_vsml.ml"
         in
        _menhir_goto_exp_vsml _menhir_env _menhir_stack _menhir_s _v) : 'freshtv462)) : 'freshtv464)
    | MenhirState99 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv471 * _menhir_state) * _menhir_state * 'tv_exp_vsml) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv467 * _menhir_state) * _menhir_state * 'tv_exp_vsml) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv465 * _menhir_state) * _menhir_state * 'tv_exp_vsml) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _, (e : 'tv_exp_vsml)) = _menhir_stack in
            let _v : 'tv_exp_vsml = 
# 104 "src/fsmcomp/debug/parser_vsml.mly"
                                   ( e )
# 1454 "src/fsmcomp/debug/parser_vsml.ml"
             in
            _menhir_goto_exp_vsml _menhir_env _menhir_stack _menhir_s _v) : 'freshtv466)) : 'freshtv468)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv469 * _menhir_state) * _menhir_state * 'tv_exp_vsml) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv470)) : 'freshtv472)
    | MenhirState52 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv485 * _menhir_state * 'tv_pstate)) * _menhir_state * 'tv_exp_vsml) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv483 * _menhir_state * 'tv_pstate)) * _menhir_state * 'tv_exp_vsml) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (x : 'tv_pstate)), _, (e : 'tv_exp_vsml)) = _menhir_stack in
        let _v : 'tv_transition_pstate_exp_vsml_ = 
# 42 "src/fsmcomp/debug/parser_vsml.mly"
                          ( (x,e) )
# 1473 "src/fsmcomp/debug/parser_vsml.ml"
         in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv481) = _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_transition_pstate_exp_vsml_) = _v in
        ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv479 * _menhir_state * 'tv_transition_pstate_exp_vsml_) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | PIPE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv473 * _menhir_state * 'tv_transition_pstate_exp_vsml_) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | CAP_IDENT _v ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState39) : 'freshtv474)
        | END ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv475 * _menhir_state * 'tv_transition_pstate_exp_vsml_) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, (x : 'tv_transition_pstate_exp_vsml_)) = _menhir_stack in
            let _v : 'tv_separated_nonempty_list_PIPE_transition_pstate_exp_vsml__ = 
# 241 "<standard.mly>"
    ( [ x ] )
# 1504 "src/fsmcomp/debug/parser_vsml.ml"
             in
            _menhir_goto_separated_nonempty_list_PIPE_transition_pstate_exp_vsml__ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv476)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv477 * _menhir_state * 'tv_transition_pstate_exp_vsml_) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv478)) : 'freshtv480)) : 'freshtv482)) : 'freshtv484)) : 'freshtv486)
    | MenhirState166 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((('freshtv489 * _menhir_state)) * _menhir_state * 'tv_option_PIPE_) * _menhir_state * 'tv_loption_separated_nonempty_list_PIPE_transition_pstate_exp_vsml___))) * _menhir_state * 'tv_exp_vsml) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((('freshtv487 * _menhir_state)) * _menhir_state * 'tv_option_PIPE_) * _menhir_state * 'tv_loption_separated_nonempty_list_PIPE_transition_pstate_exp_vsml___))) * _menhir_state * 'tv_exp_vsml) = Obj.magic _menhir_stack in
        ((let ((((_menhir_stack, _menhir_s), _, _), _, (xs : 'tv_loption_separated_nonempty_list_PIPE_transition_pstate_exp_vsml___)), _, (a : 'tv_exp_vsml)) = _menhir_stack in
        let _v : 'tv_automaton_pstate_exp_vsml_ = let ts = 
# 232 "<standard.mly>"
    ( xs )
# 1523 "src/fsmcomp/debug/parser_vsml.ml"
         in
        
# 62 "src/fsmcomp/debug/parser_vsml.mly"
  ( (ts,a) )
# 1528 "src/fsmcomp/debug/parser_vsml.ml"
         in
        _menhir_goto_automaton_pstate_exp_vsml_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv488)) : 'freshtv490)
    | _ ->
        _menhir_fail ()

and _menhir_run99 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL_LIT _v ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _v
    | CAP_IDENT _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _v
    | CASE ->
        _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState99
    | CONTINUE ->
        _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState99
    | DO ->
        _menhir_run106 _menhir_env (Obj.magic _menhir_stack) MenhirState99
    | IDENT _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _v
    | IF ->
        _menhir_run103 _menhir_env (Obj.magic _menhir_stack) MenhirState99
    | INT_LIT _v ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _v
    | LET ->
        _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState99
    | LPAREN ->
        _menhir_run99 _menhir_env (Obj.magic _menhir_stack) MenhirState99
    | MINUS ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState99
    | NOT ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState99
    | ONE ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState99
    | RPAREN ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState99
    | WILDCARD ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState99
    | ZERO ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState99
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState99

and _menhir_run100 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENT _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _v
    | WILDCARD ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState100
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState100

and _menhir_run103 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL_LIT _v ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _v
    | CAP_IDENT _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _v
    | IDENT _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _v
    | INT_LIT _v ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _v
    | LPAREN ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState103
    | MINUS ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState103
    | NOT ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState103
    | ONE ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState103
    | WILDCARD ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState103
    | ZERO ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState103
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState103

and _menhir_run106 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENT _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState106 _v
    | WILDCARD ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState106
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState106

and _menhir_run109 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL_LIT _v ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState109 _v
    | CAP_IDENT _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState109 _v
    | IDENT _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState109 _v
    | INT_LIT _v ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState109 _v
    | LPAREN ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState109
    | MINUS ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState109
    | NOT ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState109
    | ONE ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState109
    | WILDCARD ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState109
    | ZERO ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState109
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState109

and _menhir_run111 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL_LIT _v ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState111 _v
    | CAP_IDENT _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState111 _v
    | IDENT _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState111 _v
    | INT_LIT _v ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState111 _v
    | LPAREN ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState111
    | MINUS ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState111
    | NOT ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState111
    | ONE ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState111
    | WILDCARD ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState111
    | ZERO ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState111
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState111

and _menhir_goto_prim_atom_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_prim_atom_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv421) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_prim_atom_) = _v in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv419) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((p : 'tv_prim_atom_) : 'tv_prim_atom_) = _v in
    ((let _v : 'tv_atom = 
# 185 "src/fsmcomp/debug/parser_vsml.mly"
                         ( Atom.mk_prim p )
# 1713 "src/fsmcomp/debug/parser_vsml.ml"
     in
    _menhir_goto_atom _menhir_env _menhir_stack _menhir_s _v) : 'freshtv420)) : 'freshtv422)

and _menhir_run67 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_atom -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL_LIT _v ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v
    | CAP_IDENT _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v
    | IDENT _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v
    | INT_LIT _v ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v
    | LPAREN ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | MINUS ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | NOT ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | ONE ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | WILDCARD ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | ZERO ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState67

and _menhir_run73 : _menhir_env -> ('ttv_tail * _menhir_state) * _menhir_state * 'tv_atom -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ('freshtv417 * _menhir_state) * _menhir_state * 'tv_atom) = Obj.magic _menhir_stack in
    ((let ((_menhir_stack, _menhir_s), _, (a : 'tv_atom)) = _menhir_stack in
    let _v : 'tv_atom = 
# 186 "src/fsmcomp/debug/parser_vsml.mly"
                         ( a )
# 1756 "src/fsmcomp/debug/parser_vsml.ml"
     in
    _menhir_goto_atom _menhir_env _menhir_stack _menhir_s _v) : 'freshtv418)

and _menhir_run74 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_atom -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL_LIT _v ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _v
    | CAP_IDENT _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _v
    | IDENT _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _v
    | INT_LIT _v ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _v
    | LPAREN ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | MINUS ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | NOT ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | ONE ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | WILDCARD ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | ZERO ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState74

and _menhir_run76 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_atom -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL_LIT _v ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _v
    | CAP_IDENT _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _v
    | IDENT _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _v
    | INT_LIT _v ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _v
    | LPAREN ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState76
    | MINUS ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState76
    | NOT ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState76
    | ONE ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState76
    | WILDCARD ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState76
    | ZERO ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState76
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState76

and _menhir_run78 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_atom -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL_LIT _v ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState78 _v
    | CAP_IDENT _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState78 _v
    | IDENT _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState78 _v
    | INT_LIT _v ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState78 _v
    | LPAREN ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState78
    | MINUS ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState78
    | NOT ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState78
    | ONE ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState78
    | WILDCARD ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState78
    | ZERO ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState78
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState78

and _menhir_run69 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_atom -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | INT_LIT _v ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv413 * _menhir_state * 'tv_atom)) = Obj.magic _menhir_stack in
        let (_v : (
# 14 "src/fsmcomp/debug/parser_vsml.mly"
       (int)
# 1861 "src/fsmcomp/debug/parser_vsml.ml"
        )) = _v in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv411 * _menhir_state * 'tv_atom)) = Obj.magic _menhir_stack in
        let ((n : (
# 14 "src/fsmcomp/debug/parser_vsml.mly"
       (int)
# 1869 "src/fsmcomp/debug/parser_vsml.ml"
        )) : (
# 14 "src/fsmcomp/debug/parser_vsml.mly"
       (int)
# 1873 "src/fsmcomp/debug/parser_vsml.ml"
        )) = _v in
        ((let (_menhir_stack, _menhir_s, (a : 'tv_atom)) = _menhir_stack in
        let _v : 'tv_prim_atom_ = 
# 170 "src/fsmcomp/debug/parser_vsml.mly"
                                ( match n with 
                                  | 2 -> Atom.mk_unop Atom.Mod2 a
                                  | _ -> failwith "division by ... not supported" )
# 1881 "src/fsmcomp/debug/parser_vsml.ml"
         in
        _menhir_goto_prim_atom_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv412)) : 'freshtv414)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv415 * _menhir_state * 'tv_atom)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv416)

and _menhir_run80 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_atom -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL_LIT _v ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _v
    | CAP_IDENT _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _v
    | IDENT _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _v
    | INT_LIT _v ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _v
    | LPAREN ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState80
    | MINUS ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState80
    | NOT ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState80
    | ONE ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState80
    | WILDCARD ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState80
    | ZERO ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState80
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState80

and _menhir_run82 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_atom -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL_LIT _v ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState82 _v
    | CAP_IDENT _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState82 _v
    | IDENT _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState82 _v
    | INT_LIT _v ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState82 _v
    | LPAREN ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState82
    | MINUS ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState82
    | NOT ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState82
    | ONE ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState82
    | WILDCARD ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState82
    | ZERO ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState82
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState82

and _menhir_run84 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_atom -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL_LIT _v ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _v
    | CAP_IDENT _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _v
    | IDENT _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _v
    | INT_LIT _v ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _v
    | LPAREN ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState84
    | MINUS ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState84
    | NOT ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState84
    | ONE ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState84
    | WILDCARD ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState84
    | ZERO ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState84
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState84

and _menhir_run86 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_atom -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL_LIT _v ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState86 _v
    | CAP_IDENT _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState86 _v
    | IDENT _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState86 _v
    | INT_LIT _v ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState86 _v
    | LPAREN ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState86
    | MINUS ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState86
    | NOT ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState86
    | ONE ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState86
    | WILDCARD ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState86
    | ZERO ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState86
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState86

and _menhir_run88 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_atom -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL_LIT _v ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _v
    | CAP_IDENT _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _v
    | IDENT _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _v
    | INT_LIT _v ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _v
    | LPAREN ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | MINUS ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | NOT ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | ONE ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | WILDCARD ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | ZERO ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState88

and _menhir_run90 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_atom -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL_LIT _v ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _v
    | CAP_IDENT _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _v
    | IDENT _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _v
    | INT_LIT _v ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _v
    | LPAREN ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState90
    | MINUS ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState90
    | NOT ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState90
    | ONE ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState90
    | WILDCARD ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState90
    | ZERO ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState90
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState90

and _menhir_run92 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_atom -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL_LIT _v ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _v
    | CAP_IDENT _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _v
    | IDENT _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _v
    | INT_LIT _v ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _v
    | LPAREN ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState92
    | MINUS ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState92
    | NOT ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState92
    | ONE ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState92
    | WILDCARD ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState92
    | ZERO ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState92
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState92

and _menhir_run71 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_atom -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | INT_LIT _v ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv407 * _menhir_state * 'tv_atom)) = Obj.magic _menhir_stack in
        let (_v : (
# 14 "src/fsmcomp/debug/parser_vsml.mly"
       (int)
# 2113 "src/fsmcomp/debug/parser_vsml.ml"
        )) = _v in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv405 * _menhir_state * 'tv_atom)) = Obj.magic _menhir_stack in
        let ((n : (
# 14 "src/fsmcomp/debug/parser_vsml.mly"
       (int)
# 2121 "src/fsmcomp/debug/parser_vsml.ml"
        )) : (
# 14 "src/fsmcomp/debug/parser_vsml.mly"
       (int)
# 2125 "src/fsmcomp/debug/parser_vsml.ml"
        )) = _v in
        ((let (_menhir_stack, _menhir_s, (a : 'tv_atom)) = _menhir_stack in
        let _v : 'tv_prim_atom_ = 
# 167 "src/fsmcomp/debug/parser_vsml.mly"
                                ( match n with 
                                  | 2 -> Atom.mk_unop Atom.DivBy2 a
                                  | _ -> failwith "division by ... not supported" )
# 2133 "src/fsmcomp/debug/parser_vsml.ml"
         in
        _menhir_goto_prim_atom_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv406)) : 'freshtv408)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv409 * _menhir_state * 'tv_atom)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv410)

and _menhir_run94 : _menhir_env -> ('ttv_tail * _menhir_state) * _menhir_state * 'tv_atom -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState94
    | INT ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState94
    | LPAREN ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState94
    | STD_LOGIC ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState94
    | UNIT ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState94
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState94

and _menhir_goto_ty : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_ty -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState19 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv361 * _menhir_state) * _menhir_state * 'tv_ty) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv357 * _menhir_state) * _menhir_state * 'tv_ty) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv355 * _menhir_state) * _menhir_state * 'tv_ty) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _, (ty : 'tv_ty)) = _menhir_stack in
            let _v : 'tv_ty = 
# 202 "src/fsmcomp/debug/parser_vsml.mly"
                                       ( ty )
# 2184 "src/fsmcomp/debug/parser_vsml.ml"
             in
            _menhir_goto_ty _menhir_env _menhir_stack _menhir_s _v) : 'freshtv356)) : 'freshtv358)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv359 * _menhir_state) * _menhir_state * 'tv_ty) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv360)) : 'freshtv362)
    | MenhirState16 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv375 * _menhir_state * 'tv_destination) * _menhir_state * 'tv_ident)) * _menhir_state * 'tv_ty) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv373 * _menhir_state * 'tv_destination) * _menhir_state * 'tv_ident)) * _menhir_state * 'tv_ty) = Obj.magic _menhir_stack in
        ((let (((_menhir_stack, _menhir_s, (d : 'tv_destination)), _, (x : 'tv_ident)), _, (t : 'tv_ty)) = _menhir_stack in
        let _v : 'tv_signature_decl = 
# 45 "src/fsmcomp/debug/parser_vsml.mly"
                                 ( (d,x,t) )
# 2203 "src/fsmcomp/debug/parser_vsml.ml"
         in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv371) = _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_signature_decl) = _v in
        ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv369 * _menhir_state * 'tv_signature_decl) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | COMMA ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv363 * _menhir_state * 'tv_signature_decl) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | INPUT ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState12
            | LOCAL ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState12
            | OUTPUT ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState12
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState12) : 'freshtv364)
        | END ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv365 * _menhir_state * 'tv_signature_decl) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, (x : 'tv_signature_decl)) = _menhir_stack in
            let _v : 'tv_separated_nonempty_list_COMMA_signature_decl_ = 
# 241 "<standard.mly>"
    ( [ x ] )
# 2238 "src/fsmcomp/debug/parser_vsml.ml"
             in
            _menhir_goto_separated_nonempty_list_COMMA_signature_decl_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv366)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv367 * _menhir_state * 'tv_signature_decl) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv368)) : 'freshtv370)) : 'freshtv372)) : 'freshtv374)) : 'freshtv376)
    | MenhirState29 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv381) * _menhir_state * 'tv_ident)) * 'tv_signature)) * _menhir_state * 'tv_ty) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | EQ ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((('freshtv377) * _menhir_state * 'tv_ident)) * 'tv_signature)) * _menhir_state * 'tv_ty) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | LET ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState31
            | LPAREN ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState31
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState31) : 'freshtv378)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((('freshtv379) * _menhir_state * 'tv_ident)) * 'tv_signature)) * _menhir_state * 'tv_ty) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv380)) : 'freshtv382)
    | MenhirState46 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv389 * _menhir_state * 'tv_ident)) * _menhir_state * 'tv_ty) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | COMMA ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv383 * _menhir_state * 'tv_ident)) * _menhir_state * 'tv_ty) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | IDENT _v ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _v
            | WILDCARD ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState48
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState48) : 'freshtv384)
        | RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv385 * _menhir_state * 'tv_ident)) * _menhir_state * 'tv_ty) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (x : 'tv_ident)), _, (y : 'tv_ty)) = _menhir_stack in
            let _v : 'tv_separated_nonempty_list_COMMA_separated_pair_ident_COL_ty__ = let x = 
# 175 "<standard.mly>"
    ( (x, y) )
# 2302 "src/fsmcomp/debug/parser_vsml.ml"
             in
            
# 241 "<standard.mly>"
    ( [ x ] )
# 2307 "src/fsmcomp/debug/parser_vsml.ml"
             in
            _menhir_goto_separated_nonempty_list_COMMA_separated_pair_ident_COL_ty__ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv386)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv387 * _menhir_state * 'tv_ident)) * _menhir_state * 'tv_ty) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv388)) : 'freshtv390)
    | MenhirState94 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv397 * _menhir_state) * _menhir_state * 'tv_atom)) * _menhir_state * 'tv_ty) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv393 * _menhir_state) * _menhir_state * 'tv_atom)) * _menhir_state * 'tv_ty) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv391 * _menhir_state) * _menhir_state * 'tv_atom)) * _menhir_state * 'tv_ty) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s), _, (e : 'tv_atom)), _, (t : 'tv_ty)) = _menhir_stack in
            let _v : 'tv_prim_atom_ = 
# 175 "src/fsmcomp/debug/parser_vsml.mly"
                                ( Atom.mk_ty_annot e t )
# 2333 "src/fsmcomp/debug/parser_vsml.ml"
             in
            _menhir_goto_prim_atom_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv392)) : 'freshtv394)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv395 * _menhir_state) * _menhir_state * 'tv_atom)) * _menhir_state * 'tv_ty) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv396)) : 'freshtv398)
    | MenhirState152 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv403 * _menhir_state * 'tv_ident)) * _menhir_state * 'tv_ty) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | EQ ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv399 * _menhir_state * 'tv_ident)) * _menhir_state * 'tv_ty) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | LET ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState154
            | LPAREN ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState154
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState154) : 'freshtv400)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv401 * _menhir_state * 'tv_ident)) * _menhir_state * 'tv_ty) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv402)) : 'freshtv404)
    | _ ->
        _menhir_fail ()

and _menhir_goto_destination : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_destination -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv353 * _menhir_state * 'tv_destination) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENT _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState14 _v
    | WILDCARD ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState14
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState14) : 'freshtv354)

and _menhir_fail : unit -> 'a =
  fun () ->
    Printf.fprintf stderr "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

and _menhir_run53 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv351) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _v : 'tv_std_logic = 
# 178 "src/fsmcomp/debug/parser_vsml.mly"
       ( Atom.Zero )
# 2404 "src/fsmcomp/debug/parser_vsml.ml"
     in
    _menhir_goto_std_logic _menhir_env _menhir_stack _menhir_s _v) : 'freshtv352)

and _menhir_run54 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv349) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _v : 'tv_std_logic = 
# 179 "src/fsmcomp/debug/parser_vsml.mly"
       ( Atom.One )
# 2417 "src/fsmcomp/debug/parser_vsml.ml"
     in
    _menhir_goto_std_logic _menhir_env _menhir_stack _menhir_s _v) : 'freshtv350)

and _menhir_run55 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL_LIT _v ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _v
    | CAP_IDENT _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _v
    | IDENT _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _v
    | INT_LIT _v ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _v
    | LPAREN ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | MINUS ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | NOT ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | ONE ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | WILDCARD ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | ZERO ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState55

and _menhir_run56 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL_LIT _v ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _v
    | CAP_IDENT _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _v
    | IDENT _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _v
    | INT_LIT _v ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _v
    | LPAREN ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | MINUS ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | NOT ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | ONE ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | WILDCARD ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | ZERO ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState56

and _menhir_run57 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL_LIT _v ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _v
    | CAP_IDENT _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _v
    | IDENT _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _v
    | INT_LIT _v ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _v
    | LPAREN ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | MINUS ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | NOT ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | ONE ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | RPAREN ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | WILDCARD ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | ZERO ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState57

and _menhir_run59 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 14 "src/fsmcomp/debug/parser_vsml.mly"
       (int)
# 2519 "src/fsmcomp/debug/parser_vsml.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv347) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((n : (
# 14 "src/fsmcomp/debug/parser_vsml.mly"
       (int)
# 2529 "src/fsmcomp/debug/parser_vsml.ml"
    )) : (
# 14 "src/fsmcomp/debug/parser_vsml.mly"
       (int)
# 2533 "src/fsmcomp/debug/parser_vsml.ml"
    )) = _v in
    ((let _v : 'tv_const_atom_ = 
# 161 "src/fsmcomp/debug/parser_vsml.mly"
                           ( Atom.mk_int n )
# 2538 "src/fsmcomp/debug/parser_vsml.ml"
     in
    _menhir_goto_const_atom_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv348)

and _menhir_run37 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 12 "src/fsmcomp/debug/parser_vsml.mly"
       (string)
# 2545 "src/fsmcomp/debug/parser_vsml.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv345) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((x : (
# 12 "src/fsmcomp/debug/parser_vsml.mly"
       (string)
# 2555 "src/fsmcomp/debug/parser_vsml.ml"
    )) : (
# 12 "src/fsmcomp/debug/parser_vsml.mly"
       (string)
# 2559 "src/fsmcomp/debug/parser_vsml.ml"
    )) = _v in
    ((let _v : 'tv_state = 
# 156 "src/fsmcomp/debug/parser_vsml.mly"
              ( x )
# 2564 "src/fsmcomp/debug/parser_vsml.ml"
     in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv343) = _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_state) = _v in
    ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState36 | MenhirState39 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv331 * _menhir_state * 'tv_state) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv327 * _menhir_state * 'tv_state) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | IDENT _v ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _v
            | WILDCARD ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState41
            | RPAREN ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv325) = Obj.magic _menhir_stack in
                let (_menhir_s : _menhir_state) = MenhirState41 in
                ((let _v : 'tv_loption_separated_nonempty_list_COMMA_separated_pair_ident_COL_ty___ = 
# 142 "<standard.mly>"
    ( [] )
# 2595 "src/fsmcomp/debug/parser_vsml.ml"
                 in
                _menhir_goto_loption_separated_nonempty_list_COMMA_separated_pair_ident_COL_ty___ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv326)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState41) : 'freshtv328)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv329 * _menhir_state * 'tv_state) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv330)) : 'freshtv332)
    | MenhirState143 | MenhirState129 | MenhirState124 | MenhirState111 | MenhirState109 | MenhirState103 | MenhirState55 | MenhirState56 | MenhirState92 | MenhirState90 | MenhirState88 | MenhirState86 | MenhirState84 | MenhirState82 | MenhirState80 | MenhirState78 | MenhirState76 | MenhirState74 | MenhirState67 | MenhirState57 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv333 * _menhir_state * 'tv_state) = Obj.magic _menhir_stack in
        (_menhir_reduce2 _menhir_env (Obj.magic _menhir_stack) : 'freshtv334)
    | MenhirState166 | MenhirState52 | MenhirState99 | MenhirState102 | MenhirState148 | MenhirState105 | MenhirState108 | MenhirState137 | MenhirState122 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv341 * _menhir_state * 'tv_state) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv337 * _menhir_state * 'tv_state) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | BOOL_LIT _v ->
                _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState124 _v
            | CAP_IDENT _v ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState124 _v
            | IDENT _v ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState124 _v
            | INT_LIT _v ->
                _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState124 _v
            | LPAREN ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState124
            | MINUS ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState124
            | NOT ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState124
            | ONE ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState124
            | WILDCARD ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState124
            | ZERO ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState124
            | RPAREN ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv335) = Obj.magic _menhir_stack in
                let (_menhir_s : _menhir_state) = MenhirState124 in
                ((let _v : 'tv_loption_separated_nonempty_list_COMMA_atom__ = 
# 142 "<standard.mly>"
    ( [] )
# 2652 "src/fsmcomp/debug/parser_vsml.ml"
                 in
                _menhir_goto_loption_separated_nonempty_list_COMMA_atom__ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv336)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState124) : 'freshtv338)
        | AND | COL | DIV | ELSE | END | EOF | EQ | GE | GT | IN | LAND | LE | LT | MINUS | MOD | NEQ | OTHERWISE | PIPE | PIPE_PIPE | PLUS | RPAREN | TIMES ->
            _menhir_reduce2 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv339 * _menhir_state * 'tv_state) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv340)) : 'freshtv342)
    | _ ->
        _menhir_fail ()) : 'freshtv344)) : 'freshtv346)

and _menhir_run60 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 13 "src/fsmcomp/debug/parser_vsml.mly"
       (bool)
# 2674 "src/fsmcomp/debug/parser_vsml.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv323) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((b : (
# 13 "src/fsmcomp/debug/parser_vsml.mly"
       (bool)
# 2684 "src/fsmcomp/debug/parser_vsml.ml"
    )) : (
# 13 "src/fsmcomp/debug/parser_vsml.mly"
       (bool)
# 2688 "src/fsmcomp/debug/parser_vsml.ml"
    )) = _v in
    ((let _v : 'tv_const_atom_ = 
# 159 "src/fsmcomp/debug/parser_vsml.mly"
                           ( Atom.mk_bool b )
# 2693 "src/fsmcomp/debug/parser_vsml.ml"
     in
    _menhir_goto_const_atom_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv324)

and _menhir_goto_atom : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_atom -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState57 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv199 * _menhir_state) * _menhir_state * 'tv_atom) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | COL ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack)
        | LAND ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | PIPE_PIPE ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | RPAREN ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv197 * _menhir_state) * _menhir_state * 'tv_atom) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv198)) : 'freshtv200)
    | MenhirState67 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv205 * _menhir_state * 'tv_atom)) * _menhir_state * 'tv_atom) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIV ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | AND | COL | COMMA | ELSE | END | EOF | EQ | GE | GT | IN | LAND | LE | LT | MINUS | NEQ | OTHERWISE | PIPE | PIPE_PIPE | PLUS | RPAREN | THEN | TIMES | WITH ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv201 * _menhir_state * 'tv_atom)) * _menhir_state * 'tv_atom) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (a1 : 'tv_atom)), _, (a2 : 'tv_atom)) = _menhir_stack in
            let _v : 'tv_prim_atom_ = let c = 
# 191 "src/fsmcomp/debug/parser_vsml.mly"
        ( Atom.Mul )
# 2761 "src/fsmcomp/debug/parser_vsml.ml"
             in
            
# 165 "src/fsmcomp/debug/parser_vsml.mly"
                                ( Atom.mk_binop c a1 a2 )
# 2766 "src/fsmcomp/debug/parser_vsml.ml"
             in
            _menhir_goto_prim_atom_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv202)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv203 * _menhir_state * 'tv_atom)) * _menhir_state * 'tv_atom) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv204)) : 'freshtv206)
    | MenhirState74 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv211 * _menhir_state * 'tv_atom)) * _menhir_state * 'tv_atom) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIV ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | AND | COL | COMMA | ELSE | END | EOF | EQ | GE | GT | IN | LAND | LE | LT | MINUS | NEQ | OTHERWISE | PIPE | PIPE_PIPE | PLUS | RPAREN | THEN | WITH ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv207 * _menhir_state * 'tv_atom)) * _menhir_state * 'tv_atom) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (a1 : 'tv_atom)), _, (a2 : 'tv_atom)) = _menhir_stack in
            let _v : 'tv_prim_atom_ = let c = 
# 189 "src/fsmcomp/debug/parser_vsml.mly"
       ( Atom.Add )
# 2795 "src/fsmcomp/debug/parser_vsml.ml"
             in
            
# 165 "src/fsmcomp/debug/parser_vsml.mly"
                                ( Atom.mk_binop c a1 a2 )
# 2800 "src/fsmcomp/debug/parser_vsml.ml"
             in
            _menhir_goto_prim_atom_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv208)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv209 * _menhir_state * 'tv_atom)) * _menhir_state * 'tv_atom) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv210)) : 'freshtv212)
    | MenhirState76 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv217 * _menhir_state * 'tv_atom)) * _menhir_state * 'tv_atom) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIV ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack)
        | LAND ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | AND | COL | COMMA | ELSE | END | EOF | IN | OTHERWISE | PIPE | PIPE_PIPE | RPAREN | THEN | WITH ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv213 * _menhir_state * 'tv_atom)) * _menhir_state * 'tv_atom) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (a1 : 'tv_atom)), _, (a2 : 'tv_atom)) = _menhir_stack in
            let _v : 'tv_prim_atom_ = let c = 
# 199 "src/fsmcomp/debug/parser_vsml.mly"
            ( Atom.Or )
# 2847 "src/fsmcomp/debug/parser_vsml.ml"
             in
            
# 165 "src/fsmcomp/debug/parser_vsml.mly"
                                ( Atom.mk_binop c a1 a2 )
# 2852 "src/fsmcomp/debug/parser_vsml.ml"
             in
            _menhir_goto_prim_atom_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv214)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv215 * _menhir_state * 'tv_atom)) * _menhir_state * 'tv_atom) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv216)) : 'freshtv218)
    | MenhirState78 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv223 * _menhir_state * 'tv_atom)) * _menhir_state * 'tv_atom) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIV ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | AND | COL | COMMA | ELSE | END | EOF | EQ | GE | GT | IN | LAND | LE | LT | NEQ | OTHERWISE | PIPE | PIPE_PIPE | RPAREN | THEN | WITH ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv219 * _menhir_state * 'tv_atom)) * _menhir_state * 'tv_atom) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (a1 : 'tv_atom)), _, (a2 : 'tv_atom)) = _menhir_stack in
            let _v : 'tv_prim_atom_ = let c = 
# 197 "src/fsmcomp/debug/parser_vsml.mly"
      ( Atom.Neq )
# 2885 "src/fsmcomp/debug/parser_vsml.ml"
             in
            
# 165 "src/fsmcomp/debug/parser_vsml.mly"
                                ( Atom.mk_binop c a1 a2 )
# 2890 "src/fsmcomp/debug/parser_vsml.ml"
             in
            _menhir_goto_prim_atom_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv220)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv221 * _menhir_state * 'tv_atom)) * _menhir_state * 'tv_atom) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv222)) : 'freshtv224)
    | MenhirState80 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv229 * _menhir_state * 'tv_atom)) * _menhir_state * 'tv_atom) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIV ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | AND | COL | COMMA | ELSE | END | EOF | EQ | GE | GT | IN | LAND | LE | LT | MINUS | NEQ | OTHERWISE | PIPE | PIPE_PIPE | PLUS | RPAREN | THEN | WITH ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv225 * _menhir_state * 'tv_atom)) * _menhir_state * 'tv_atom) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (a1 : 'tv_atom)), _, (a2 : 'tv_atom)) = _menhir_stack in
            let _v : 'tv_prim_atom_ = let c = 
# 190 "src/fsmcomp/debug/parser_vsml.mly"
        ( Atom.Sub )
# 2919 "src/fsmcomp/debug/parser_vsml.ml"
             in
            
# 165 "src/fsmcomp/debug/parser_vsml.mly"
                                ( Atom.mk_binop c a1 a2 )
# 2924 "src/fsmcomp/debug/parser_vsml.ml"
             in
            _menhir_goto_prim_atom_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv226)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv227 * _menhir_state * 'tv_atom)) * _menhir_state * 'tv_atom) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv228)) : 'freshtv230)
    | MenhirState82 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv235 * _menhir_state * 'tv_atom)) * _menhir_state * 'tv_atom) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIV ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | AND | COL | COMMA | ELSE | END | EOF | EQ | GE | GT | IN | LAND | LE | LT | NEQ | OTHERWISE | PIPE | PIPE_PIPE | RPAREN | THEN | WITH ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv231 * _menhir_state * 'tv_atom)) * _menhir_state * 'tv_atom) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (a1 : 'tv_atom)), _, (a2 : 'tv_atom)) = _menhir_stack in
            let _v : 'tv_prim_atom_ = let c = 
# 192 "src/fsmcomp/debug/parser_vsml.mly"
     ( Atom.Lt )
# 2957 "src/fsmcomp/debug/parser_vsml.ml"
             in
            
# 165 "src/fsmcomp/debug/parser_vsml.mly"
                                ( Atom.mk_binop c a1 a2 )
# 2962 "src/fsmcomp/debug/parser_vsml.ml"
             in
            _menhir_goto_prim_atom_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv232)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv233 * _menhir_state * 'tv_atom)) * _menhir_state * 'tv_atom) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv234)) : 'freshtv236)
    | MenhirState84 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv241 * _menhir_state * 'tv_atom)) * _menhir_state * 'tv_atom) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIV ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | AND | COL | COMMA | ELSE | END | EOF | EQ | GE | GT | IN | LAND | LE | LT | NEQ | OTHERWISE | PIPE | PIPE_PIPE | RPAREN | THEN | WITH ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv237 * _menhir_state * 'tv_atom)) * _menhir_state * 'tv_atom) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (a1 : 'tv_atom)), _, (a2 : 'tv_atom)) = _menhir_stack in
            let _v : 'tv_prim_atom_ = let c = 
# 193 "src/fsmcomp/debug/parser_vsml.mly"
     ( Atom.Le )
# 2995 "src/fsmcomp/debug/parser_vsml.ml"
             in
            
# 165 "src/fsmcomp/debug/parser_vsml.mly"
                                ( Atom.mk_binop c a1 a2 )
# 3000 "src/fsmcomp/debug/parser_vsml.ml"
             in
            _menhir_goto_prim_atom_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv238)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv239 * _menhir_state * 'tv_atom)) * _menhir_state * 'tv_atom) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv240)) : 'freshtv242)
    | MenhirState86 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv247 * _menhir_state * 'tv_atom)) * _menhir_state * 'tv_atom) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIV ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | AND | COL | COMMA | ELSE | END | EOF | IN | LAND | OTHERWISE | PIPE | PIPE_PIPE | RPAREN | THEN | WITH ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv243 * _menhir_state * 'tv_atom)) * _menhir_state * 'tv_atom) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (a1 : 'tv_atom)), _, (a2 : 'tv_atom)) = _menhir_stack in
            let _v : 'tv_prim_atom_ = let c = 
# 198 "src/fsmcomp/debug/parser_vsml.mly"
       ( Atom.And )
# 3045 "src/fsmcomp/debug/parser_vsml.ml"
             in
            
# 165 "src/fsmcomp/debug/parser_vsml.mly"
                                ( Atom.mk_binop c a1 a2 )
# 3050 "src/fsmcomp/debug/parser_vsml.ml"
             in
            _menhir_goto_prim_atom_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv244)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv245 * _menhir_state * 'tv_atom)) * _menhir_state * 'tv_atom) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv246)) : 'freshtv248)
    | MenhirState88 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv253 * _menhir_state * 'tv_atom)) * _menhir_state * 'tv_atom) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIV ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | AND | COL | COMMA | ELSE | END | EOF | EQ | GE | GT | IN | LAND | LE | LT | NEQ | OTHERWISE | PIPE | PIPE_PIPE | RPAREN | THEN | WITH ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv249 * _menhir_state * 'tv_atom)) * _menhir_state * 'tv_atom) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (a1 : 'tv_atom)), _, (a2 : 'tv_atom)) = _menhir_stack in
            let _v : 'tv_prim_atom_ = let c = 
# 194 "src/fsmcomp/debug/parser_vsml.mly"
     ( Atom.Gt )
# 3083 "src/fsmcomp/debug/parser_vsml.ml"
             in
            
# 165 "src/fsmcomp/debug/parser_vsml.mly"
                                ( Atom.mk_binop c a1 a2 )
# 3088 "src/fsmcomp/debug/parser_vsml.ml"
             in
            _menhir_goto_prim_atom_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv250)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv251 * _menhir_state * 'tv_atom)) * _menhir_state * 'tv_atom) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv252)) : 'freshtv254)
    | MenhirState90 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv259 * _menhir_state * 'tv_atom)) * _menhir_state * 'tv_atom) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIV ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | AND | COL | COMMA | ELSE | END | EOF | EQ | GE | GT | IN | LAND | LE | LT | NEQ | OTHERWISE | PIPE | PIPE_PIPE | RPAREN | THEN | WITH ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv255 * _menhir_state * 'tv_atom)) * _menhir_state * 'tv_atom) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (a1 : 'tv_atom)), _, (a2 : 'tv_atom)) = _menhir_stack in
            let _v : 'tv_prim_atom_ = let c = 
# 195 "src/fsmcomp/debug/parser_vsml.mly"
     ( Atom.Ge )
# 3121 "src/fsmcomp/debug/parser_vsml.ml"
             in
            
# 165 "src/fsmcomp/debug/parser_vsml.mly"
                                ( Atom.mk_binop c a1 a2 )
# 3126 "src/fsmcomp/debug/parser_vsml.ml"
             in
            _menhir_goto_prim_atom_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv256)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv257 * _menhir_state * 'tv_atom)) * _menhir_state * 'tv_atom) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv258)) : 'freshtv260)
    | MenhirState92 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv265 * _menhir_state * 'tv_atom)) * _menhir_state * 'tv_atom) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIV ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | AND | COL | COMMA | ELSE | END | EOF | EQ | GE | GT | IN | LAND | LE | LT | NEQ | OTHERWISE | PIPE | PIPE_PIPE | RPAREN | THEN | WITH ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv261 * _menhir_state * 'tv_atom)) * _menhir_state * 'tv_atom) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (a1 : 'tv_atom)), _, (a2 : 'tv_atom)) = _menhir_stack in
            let _v : 'tv_prim_atom_ = let c = 
# 196 "src/fsmcomp/debug/parser_vsml.mly"
     ( Atom.Eq )
# 3159 "src/fsmcomp/debug/parser_vsml.ml"
             in
            
# 165 "src/fsmcomp/debug/parser_vsml.mly"
                                ( Atom.mk_binop c a1 a2 )
# 3164 "src/fsmcomp/debug/parser_vsml.ml"
             in
            _menhir_goto_prim_atom_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv262)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv263 * _menhir_state * 'tv_atom)) * _menhir_state * 'tv_atom) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv264)) : 'freshtv266)
    | MenhirState56 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv271 * _menhir_state) * _menhir_state * 'tv_atom) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIV ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | AND | COL | COMMA | ELSE | END | EOF | EQ | GE | GT | IN | LAND | LE | LT | MINUS | NEQ | OTHERWISE | PIPE | PIPE_PIPE | PLUS | RPAREN | THEN | TIMES | WITH ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv267 * _menhir_state) * _menhir_state * 'tv_atom) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _, (a : 'tv_atom)) = _menhir_stack in
            let _v : 'tv_prim_atom_ = 
# 174 "src/fsmcomp/debug/parser_vsml.mly"
                                ( Atom.mk_unop Atom.Uminus a )
# 3191 "src/fsmcomp/debug/parser_vsml.ml"
             in
            _menhir_goto_prim_atom_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv268)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv269 * _menhir_state) * _menhir_state * 'tv_atom) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv270)) : 'freshtv272)
    | MenhirState55 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv277 * _menhir_state) * _menhir_state * 'tv_atom) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIV ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | AND | COL | COMMA | ELSE | END | EOF | EQ | GE | GT | IN | LAND | LE | LT | MINUS | NEQ | OTHERWISE | PIPE | PIPE_PIPE | PLUS | RPAREN | THEN | TIMES | WITH ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv273 * _menhir_state) * _menhir_state * 'tv_atom) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _, (a : 'tv_atom)) = _menhir_stack in
            let _v : 'tv_prim_atom_ = 
# 166 "src/fsmcomp/debug/parser_vsml.mly"
                                ( Atom.mk_unop Atom.Not a  )
# 3218 "src/fsmcomp/debug/parser_vsml.ml"
             in
            _menhir_goto_prim_atom_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv274)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv275 * _menhir_state) * _menhir_state * 'tv_atom) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv276)) : 'freshtv278)
    | MenhirState103 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv283 * _menhir_state) * _menhir_state * 'tv_atom) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIV ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack)
        | LAND ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | PIPE_PIPE ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | THEN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv279 * _menhir_state) * _menhir_state * 'tv_atom) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | BOOL_LIT _v ->
                _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState105 _v
            | CAP_IDENT _v ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState105 _v
            | CASE ->
                _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState105
            | CONTINUE ->
                _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState105
            | DO ->
                _menhir_run106 _menhir_env (Obj.magic _menhir_stack) MenhirState105
            | IDENT _v ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState105 _v
            | IF ->
                _menhir_run103 _menhir_env (Obj.magic _menhir_stack) MenhirState105
            | INT_LIT _v ->
                _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState105 _v
            | LET ->
                _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState105
            | LPAREN ->
                _menhir_run99 _menhir_env (Obj.magic _menhir_stack) MenhirState105
            | MINUS ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState105
            | NOT ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState105
            | ONE ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState105
            | WILDCARD ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState105
            | ZERO ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState105
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState105) : 'freshtv280)
        | TIMES ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv281 * _menhir_state) * _menhir_state * 'tv_atom) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv282)) : 'freshtv284)
    | MenhirState109 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv289 * _menhir_state) * _menhir_state * 'tv_atom) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIV ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack)
        | LAND ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | PIPE_PIPE ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | AND | ELSE | END | EOF | IN | OTHERWISE | PIPE | RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv285 * _menhir_state) * _menhir_state * 'tv_atom) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _, (a : 'tv_atom)) = _menhir_stack in
            let _v : 'tv_exp_vsml = 
# 111 "src/fsmcomp/debug/parser_vsml.mly"
                                  ( VSML.Continue a )
# 3346 "src/fsmcomp/debug/parser_vsml.ml"
             in
            _menhir_goto_exp_vsml _menhir_env _menhir_stack _menhir_s _v) : 'freshtv286)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv287 * _menhir_state) * _menhir_state * 'tv_atom) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv288)) : 'freshtv290)
    | MenhirState111 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv295 * _menhir_state) * _menhir_state * 'tv_atom) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIV ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack)
        | LAND ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | PIPE_PIPE ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | WITH ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv291 * _menhir_state) * _menhir_state * 'tv_atom) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | PIPE ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState113
            | BOOL_LIT _ | INT_LIT _ | LPAREN | ONE | ZERO ->
                _menhir_reduce41 _menhir_env (Obj.magic _menhir_stack) MenhirState113
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState113) : 'freshtv292)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv293 * _menhir_state) * _menhir_state * 'tv_atom) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv294)) : 'freshtv296)
    | MenhirState129 | MenhirState124 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv303 * _menhir_state * 'tv_atom) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | COMMA ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv297 * _menhir_state * 'tv_atom) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | BOOL_LIT _v ->
                _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState129 _v
            | CAP_IDENT _v ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState129 _v
            | IDENT _v ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState129 _v
            | INT_LIT _v ->
                _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState129 _v
            | LPAREN ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState129
            | MINUS ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState129
            | NOT ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState129
            | ONE ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState129
            | WILDCARD ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState129
            | ZERO ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState129
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState129) : 'freshtv298)
        | DIV ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack)
        | LAND ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | PIPE_PIPE ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv299 * _menhir_state * 'tv_atom) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, (x : 'tv_atom)) = _menhir_stack in
            let _v : 'tv_separated_nonempty_list_COMMA_atom_ = 
# 241 "<standard.mly>"
    ( [ x ] )
# 3478 "src/fsmcomp/debug/parser_vsml.ml"
             in
            _menhir_goto_separated_nonempty_list_COMMA_atom_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv300)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv301 * _menhir_state * 'tv_atom) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv302)) : 'freshtv304)
    | MenhirState166 | MenhirState52 | MenhirState102 | MenhirState105 | MenhirState148 | MenhirState108 | MenhirState137 | MenhirState122 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv309 * _menhir_state * 'tv_atom) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIV ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack)
        | LAND ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | PIPE_PIPE ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | AND | ELSE | END | EOF | IN | OTHERWISE | PIPE | RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv305 * _menhir_state * 'tv_atom) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, (a : 'tv_atom)) = _menhir_stack in
            let _v : 'tv_exp_vsml = 
# 105 "src/fsmcomp/debug/parser_vsml.mly"
                                   ( VSML.Atom a )
# 3527 "src/fsmcomp/debug/parser_vsml.ml"
             in
            _menhir_goto_exp_vsml _menhir_env _menhir_stack _menhir_s _v) : 'freshtv306)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv307 * _menhir_state * 'tv_atom) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv308)) : 'freshtv310)
    | MenhirState143 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv317 * _menhir_state * 'tv_ident)) * _menhir_state * 'tv_atom) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | AND ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv311 * _menhir_state * 'tv_ident)) * _menhir_state * 'tv_atom) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | IDENT _v ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState145 _v
            | WILDCARD ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState145
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState145) : 'freshtv312)
        | DIV ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack)
        | LAND ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | PIPE_PIPE ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | THEN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv313 * _menhir_state * 'tv_ident)) * _menhir_state * 'tv_atom) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (x : 'tv_ident)), _, (y : 'tv_atom)) = _menhir_stack in
            let _v : 'tv_separated_nonempty_list_AND_separated_pair_ident_COLONEQ_atom__ = let x = 
# 175 "<standard.mly>"
    ( (x, y) )
# 3590 "src/fsmcomp/debug/parser_vsml.ml"
             in
            
# 241 "<standard.mly>"
    ( [ x ] )
# 3595 "src/fsmcomp/debug/parser_vsml.ml"
             in
            _menhir_goto_separated_nonempty_list_AND_separated_pair_ident_COLONEQ_atom__ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv314)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv315 * _menhir_state * 'tv_ident)) * _menhir_state * 'tv_atom) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv316)) : 'freshtv318)
    | MenhirState99 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv321 * _menhir_state) * _menhir_state * 'tv_atom) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | COL ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack)
        | LAND ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | PIPE_PIPE ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | RPAREN ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv319 * _menhir_state) * _menhir_state * 'tv_atom) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv320)) : 'freshtv322)
    | _ ->
        _menhir_fail ()

and _menhir_run17 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv195) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _v : 'tv_ty = 
# 206 "src/fsmcomp/debug/parser_vsml.mly"
                                       ( TConst TUnit )
# 3660 "src/fsmcomp/debug/parser_vsml.ml"
     in
    _menhir_goto_ty _menhir_env _menhir_stack _menhir_s _v) : 'freshtv196)

and _menhir_run18 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv193) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _v : 'tv_ty = 
# 203 "src/fsmcomp/debug/parser_vsml.mly"
                                       ( TConst TStd_logic )
# 3673 "src/fsmcomp/debug/parser_vsml.ml"
     in
    _menhir_goto_ty _menhir_env _menhir_stack _menhir_s _v) : 'freshtv194)

and _menhir_run19 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | INT ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | LPAREN ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | STD_LOGIC ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | UNIT ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState19

and _menhir_run20 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv191) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _v : 'tv_ty = 
# 204 "src/fsmcomp/debug/parser_vsml.mly"
                                       ( TConst TInt )
# 3707 "src/fsmcomp/debug/parser_vsml.ml"
     in
    _menhir_goto_ty _menhir_env _menhir_stack _menhir_s _v) : 'freshtv192)

and _menhir_run21 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv189) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _v : 'tv_ty = 
# 205 "src/fsmcomp/debug/parser_vsml.mly"
                                       ( TConst TBool )
# 3720 "src/fsmcomp/debug/parser_vsml.ml"
     in
    _menhir_goto_ty _menhir_env _menhir_stack _menhir_s _v) : 'freshtv190)

and _menhir_goto_loption_separated_nonempty_list_COMMA_signature_decl__ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_loption_separated_nonempty_list_COMMA_signature_decl__ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ('freshtv187) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_signature_decl__) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | END ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv183) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_signature_decl__) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv181) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_signature_decl__) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, (xs : 'tv_loption_separated_nonempty_list_COMMA_signature_decl__)) = _menhir_stack in
        let _v : 'tv_signature = let ds = 
# 232 "<standard.mly>"
    ( xs )
# 3742 "src/fsmcomp/debug/parser_vsml.ml"
         in
        
# 53 "src/fsmcomp/debug/parser_vsml.mly"
                                                  ( ds )
# 3747 "src/fsmcomp/debug/parser_vsml.ml"
         in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv179) = _menhir_stack in
        let (_v : 'tv_signature) = _v in
        ((let _menhir_stack = (_menhir_stack, _v) in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv177) * _menhir_state * 'tv_ident)) * 'tv_signature) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RETURN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv173) * _menhir_state * 'tv_ident)) * 'tv_signature) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | BOOL ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState29
            | INT ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState29
            | LPAREN ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState29
            | STD_LOGIC ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState29
            | UNIT ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState29
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState29) : 'freshtv174)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv175) * _menhir_state * 'tv_ident)) * 'tv_signature) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv176)) : 'freshtv178)) : 'freshtv180)) : 'freshtv182)) : 'freshtv184)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv185) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_signature_decl__) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv186)) : 'freshtv188)

and _menhir_run8 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv171) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _v : 'tv_destination = 
# 50 "src/fsmcomp/debug/parser_vsml.mly"
         ( Out )
# 3802 "src/fsmcomp/debug/parser_vsml.ml"
     in
    _menhir_goto_destination _menhir_env _menhir_stack _menhir_s _v) : 'freshtv172)

and _menhir_run9 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv169) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _v : 'tv_destination = 
# 48 "src/fsmcomp/debug/parser_vsml.mly"
        ( Local )
# 3815 "src/fsmcomp/debug/parser_vsml.ml"
     in
    _menhir_goto_destination _menhir_env _menhir_stack _menhir_s _v) : 'freshtv170)

and _menhir_run10 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv167) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _v : 'tv_destination = 
# 49 "src/fsmcomp/debug/parser_vsml.mly"
        ( In )
# 3828 "src/fsmcomp/debug/parser_vsml.ml"
     in
    _menhir_goto_destination _menhir_env _menhir_stack _menhir_s _v) : 'freshtv168)

and _menhir_goto_vsml : _menhir_env -> 'ttv_tail -> (
# 37 "src/fsmcomp/debug/parser_vsml.mly"
       (Kast.VSML.circuit)
# 3835 "src/fsmcomp/debug/parser_vsml.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv165) = Obj.magic _menhir_stack in
    let (_v : (
# 37 "src/fsmcomp/debug/parser_vsml.mly"
       (Kast.VSML.circuit)
# 3843 "src/fsmcomp/debug/parser_vsml.ml"
    )) = _v in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv163) = Obj.magic _menhir_stack in
    let ((_1 : (
# 37 "src/fsmcomp/debug/parser_vsml.mly"
       (Kast.VSML.circuit)
# 3850 "src/fsmcomp/debug/parser_vsml.ml"
    )) : (
# 37 "src/fsmcomp/debug/parser_vsml.mly"
       (Kast.VSML.circuit)
# 3854 "src/fsmcomp/debug/parser_vsml.ml"
    )) = _v in
    (Obj.magic _1 : 'freshtv164)) : 'freshtv166)

and _menhir_goto_ident : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_ident -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState2 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv133) * _menhir_state * 'tv_ident) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | COL ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv129) * _menhir_state * 'tv_ident) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | SIG ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv125) = Obj.magic _menhir_stack in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | INPUT ->
                    _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState7
                | LOCAL ->
                    _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState7
                | OUTPUT ->
                    _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState7
                | END ->
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : 'freshtv123) = Obj.magic _menhir_stack in
                    let (_menhir_s : _menhir_state) = MenhirState7 in
                    ((let _v : 'tv_loption_separated_nonempty_list_COMMA_signature_decl__ = 
# 142 "<standard.mly>"
    ( [] )
# 3893 "src/fsmcomp/debug/parser_vsml.ml"
                     in
                    _menhir_goto_loption_separated_nonempty_list_COMMA_signature_decl__ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv124)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState7) : 'freshtv126)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv127) * _menhir_state * 'tv_ident)) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv128)) : 'freshtv130)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv131) * _menhir_state * 'tv_ident) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv132)) : 'freshtv134)
    | MenhirState14 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv139 * _menhir_state * 'tv_destination) * _menhir_state * 'tv_ident) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | COL ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv135 * _menhir_state * 'tv_destination) * _menhir_state * 'tv_ident) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | BOOL ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState16
            | INT ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState16
            | LPAREN ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState16
            | STD_LOGIC ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState16
            | UNIT ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState16
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState16) : 'freshtv136)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv137 * _menhir_state * 'tv_destination) * _menhir_state * 'tv_ident) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv138)) : 'freshtv140)
    | MenhirState48 | MenhirState41 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv145 * _menhir_state * 'tv_ident) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | COL ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv141 * _menhir_state * 'tv_ident) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | BOOL ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState46
            | INT ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState46
            | LPAREN ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState46
            | STD_LOGIC ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState46
            | UNIT ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState46
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState46) : 'freshtv142)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv143 * _menhir_state * 'tv_ident) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv144)) : 'freshtv146)
    | MenhirState166 | MenhirState52 | MenhirState99 | MenhirState102 | MenhirState148 | MenhirState105 | MenhirState143 | MenhirState108 | MenhirState137 | MenhirState122 | MenhirState129 | MenhirState124 | MenhirState111 | MenhirState109 | MenhirState103 | MenhirState55 | MenhirState56 | MenhirState92 | MenhirState90 | MenhirState88 | MenhirState86 | MenhirState84 | MenhirState82 | MenhirState80 | MenhirState78 | MenhirState76 | MenhirState74 | MenhirState67 | MenhirState57 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv149 * _menhir_state * 'tv_ident) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv147 * _menhir_state * 'tv_ident) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, (x : 'tv_ident)) = _menhir_stack in
        let _v : 'tv_atom = 
# 182 "src/fsmcomp/debug/parser_vsml.mly"
                         ( Atom.Var x )
# 3989 "src/fsmcomp/debug/parser_vsml.ml"
         in
        _menhir_goto_atom _menhir_env _menhir_stack _menhir_s _v) : 'freshtv148)) : 'freshtv150)
    | MenhirState145 | MenhirState106 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv155 * _menhir_state * 'tv_ident) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | COLONEQ ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv151 * _menhir_state * 'tv_ident) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | BOOL_LIT _v ->
                _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState143 _v
            | CAP_IDENT _v ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState143 _v
            | IDENT _v ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState143 _v
            | INT_LIT _v ->
                _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState143 _v
            | LPAREN ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState143
            | MINUS ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState143
            | NOT ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState143
            | ONE ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState143
            | WILDCARD ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState143
            | ZERO ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState143
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState143) : 'freshtv152)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv153 * _menhir_state * 'tv_ident) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv154)) : 'freshtv156)
    | MenhirState156 | MenhirState100 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv161 * _menhir_state * 'tv_ident) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | COL ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv157 * _menhir_state * 'tv_ident) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | BOOL ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState152
            | INT ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState152
            | LPAREN ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState152
            | STD_LOGIC ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState152
            | UNIT ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState152
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState152) : 'freshtv158)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv159 * _menhir_state * 'tv_ident) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv160)) : 'freshtv162)
    | _ ->
        _menhir_fail ()

and _menhir_error0 : _menhir_env -> 'ttv_tail -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv121) = Obj.magic _menhir_stack in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv119) = Obj.magic _menhir_stack in
    ((let _v : (
# 37 "src/fsmcomp/debug/parser_vsml.mly"
       (Kast.VSML.circuit)
# 4080 "src/fsmcomp/debug/parser_vsml.ml"
    ) = 
# 152 "src/fsmcomp/debug/parser_vsml.mly"
        (failwith "bim")
# 4084 "src/fsmcomp/debug/parser_vsml.ml"
     in
    _menhir_goto_vsml _menhir_env _menhir_stack _v) : 'freshtv120)) : 'freshtv122)

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState166 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv9 * _menhir_state)) * _menhir_state * 'tv_option_PIPE_) * _menhir_state * 'tv_loption_separated_nonempty_list_PIPE_transition_pstate_exp_vsml___))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv10)
    | MenhirState156 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv11 * _menhir_state * 'tv_ident)) * _menhir_state * 'tv_ty)) * _menhir_state * 'tv_automaton_vsml)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv12)
    | MenhirState154 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv13 * _menhir_state * 'tv_ident)) * _menhir_state * 'tv_ty)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv14)
    | MenhirState152 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv15 * _menhir_state * 'tv_ident)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv16)
    | MenhirState148 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv17 * _menhir_state) * _menhir_state * 'tv_atom)) * _menhir_state * 'tv_exp_vsml)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv18)
    | MenhirState145 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv19 * _menhir_state * 'tv_ident)) * _menhir_state * 'tv_atom)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv20)
    | MenhirState143 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv21 * _menhir_state * 'tv_ident)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv22)
    | MenhirState139 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv23 * _menhir_state * 'tv_constant)) * _menhir_state * 'tv_exp_vsml)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv24)
    | MenhirState137 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv25 * _menhir_state * 'tv_constant)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv26)
    | MenhirState129 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv27 * _menhir_state * 'tv_atom)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv28)
    | MenhirState124 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv29 * _menhir_state * 'tv_state)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv30)
    | MenhirState122 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((('freshtv31 * _menhir_state) * _menhir_state * 'tv_atom)) * _menhir_state * 'tv_option_PIPE_) * _menhir_state * 'tv_separated_nonempty_list_PIPE_separated_pair_constant_RIGHT_ARROW_exp_vsml__) * _menhir_state * 'tv_option_PIPE_)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv32)
    | MenhirState120 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv33 * _menhir_state) * _menhir_state * 'tv_atom)) * _menhir_state * 'tv_option_PIPE_) * _menhir_state * 'tv_separated_nonempty_list_PIPE_separated_pair_constant_RIGHT_ARROW_exp_vsml__) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv34)
    | MenhirState114 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv35 * _menhir_state) * _menhir_state * 'tv_atom)) * _menhir_state * 'tv_option_PIPE_) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv36)
    | MenhirState113 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv37 * _menhir_state) * _menhir_state * 'tv_atom)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv38)
    | MenhirState111 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv39 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv40)
    | MenhirState109 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv41 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv42)
    | MenhirState108 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv43 * _menhir_state) * _menhir_state * 'tv_separated_nonempty_list_AND_separated_pair_ident_COLONEQ_atom__)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv44)
    | MenhirState106 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv45 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv46)
    | MenhirState105 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv47 * _menhir_state) * _menhir_state * 'tv_atom)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv48)
    | MenhirState103 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv49 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv50)
    | MenhirState102 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv51 * _menhir_state) * _menhir_state * 'tv_separated_nonempty_list_AND_separated_pair_separated_pair_ident_COL_ty__EQ_automaton_vsml__)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv52)
    | MenhirState100 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv53 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv54)
    | MenhirState99 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv55 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv56)
    | MenhirState94 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv57 * _menhir_state) * _menhir_state * 'tv_atom)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv58)
    | MenhirState92 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv59 * _menhir_state * 'tv_atom)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv60)
    | MenhirState90 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv61 * _menhir_state * 'tv_atom)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv62)
    | MenhirState88 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv63 * _menhir_state * 'tv_atom)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv64)
    | MenhirState86 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv65 * _menhir_state * 'tv_atom)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv66)
    | MenhirState84 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv67 * _menhir_state * 'tv_atom)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv68)
    | MenhirState82 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv69 * _menhir_state * 'tv_atom)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv70)
    | MenhirState80 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv71 * _menhir_state * 'tv_atom)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv72)
    | MenhirState78 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv73 * _menhir_state * 'tv_atom)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv74)
    | MenhirState76 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv75 * _menhir_state * 'tv_atom)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv76)
    | MenhirState74 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv77 * _menhir_state * 'tv_atom)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv78)
    | MenhirState67 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv79 * _menhir_state * 'tv_atom)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv80)
    | MenhirState57 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv81 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv82)
    | MenhirState56 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv83 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv84)
    | MenhirState55 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv85 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv86)
    | MenhirState52 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv87 * _menhir_state * 'tv_pstate)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv88)
    | MenhirState48 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv89 * _menhir_state * 'tv_ident)) * _menhir_state * 'tv_ty)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv90)
    | MenhirState46 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv91 * _menhir_state * 'tv_ident)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv92)
    | MenhirState41 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv93 * _menhir_state * 'tv_state)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv94)
    | MenhirState39 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv95 * _menhir_state * 'tv_transition_pstate_exp_vsml_)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv96)
    | MenhirState36 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv97 * _menhir_state)) * _menhir_state * 'tv_option_PIPE_) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv98)
    | MenhirState34 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv99 * _menhir_state)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv100)
    | MenhirState32 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv101 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv102)
    | MenhirState31 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((('freshtv103) * _menhir_state * 'tv_ident)) * 'tv_signature)) * _menhir_state * 'tv_ty)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv104)
    | MenhirState29 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv105) * _menhir_state * 'tv_ident)) * 'tv_signature)) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv106)
    | MenhirState19 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv107 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv108)
    | MenhirState16 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv109 * _menhir_state * 'tv_destination) * _menhir_state * 'tv_ident)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv110)
    | MenhirState14 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv111 * _menhir_state * 'tv_destination) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv112)
    | MenhirState12 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv113 * _menhir_state * 'tv_signature_decl)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv114)
    | MenhirState7 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv115) = Obj.magic _menhir_stack in
        (raise _eRR : 'freshtv116)
    | MenhirState2 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv117) = Obj.magic _menhir_stack in
        (_menhir_error0 _menhir_env (Obj.magic _menhir_stack) : 'freshtv118)

and _menhir_run3 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv7) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _v : 'tv_ident = 
# 210 "src/fsmcomp/debug/parser_vsml.mly"
           ( Gensym.gensym "wildcard" )
# 4374 "src/fsmcomp/debug/parser_vsml.ml"
     in
    _menhir_goto_ident _menhir_env _menhir_stack _menhir_s _v) : 'freshtv8)

and _menhir_run4 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 12 "src/fsmcomp/debug/parser_vsml.mly"
       (string)
# 4381 "src/fsmcomp/debug/parser_vsml.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv5) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((x : (
# 12 "src/fsmcomp/debug/parser_vsml.mly"
       (string)
# 4391 "src/fsmcomp/debug/parser_vsml.ml"
    )) : (
# 12 "src/fsmcomp/debug/parser_vsml.mly"
       (string)
# 4395 "src/fsmcomp/debug/parser_vsml.ml"
    )) = _v in
    ((let _v : 'tv_ident = 
# 209 "src/fsmcomp/debug/parser_vsml.mly"
          ( x )
# 4400 "src/fsmcomp/debug/parser_vsml.ml"
     in
    _menhir_goto_ident _menhir_env _menhir_stack _menhir_s _v) : 'freshtv6)

and _menhir_discard : _menhir_env -> _menhir_env =
  fun _menhir_env ->
    let lexer = _menhir_env._menhir_lexer in
    let lexbuf = _menhir_env._menhir_lexbuf in
    let _tok = lexer lexbuf in
    {
      _menhir_lexer = lexer;
      _menhir_lexbuf = lexbuf;
      _menhir_token = _tok;
      _menhir_error = false;
    }

and vsml : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (
# 37 "src/fsmcomp/debug/parser_vsml.mly"
       (Kast.VSML.circuit)
# 4419 "src/fsmcomp/debug/parser_vsml.ml"
) =
  fun lexer lexbuf ->
    let _menhir_env = {
      _menhir_lexer = lexer;
      _menhir_lexbuf = lexbuf;
      _menhir_token = Obj.magic ();
      _menhir_error = false;
    } in
    Obj.magic (let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv3) = ((), _menhir_env._menhir_lexbuf.Lexing.lex_curr_p) in
    ((let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CIRCUIT ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv1) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | IDENT _v ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState2 _v
        | WILDCARD ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState2
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState2) : 'freshtv2)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_error0 _menhir_env (Obj.magic _menhir_stack)) : 'freshtv4))

# 269 "<standard.mly>"
  

# 4455 "src/fsmcomp/debug/parser_vsml.ml"
