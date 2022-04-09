
module MenhirBasics = struct
  
  exception Error
  
  type token = 
    | WHILE
    | VOID
    | SUPEQ
    | SUP
    | SET
    | SEMI
    | RPAR
    | RETURN
    | PUTCHAR
    | OR
    | NEG
    | MUL
    | LPAR
    | INT
    | INFEQ
    | INF
    | IF
    | IDENT of (
# 14 "minic_parser.mly"
       (string)
# 28 "minic_parser.ml"
  )
    | EQUAL
    | EOF
    | END
    | ELSE
    | DIV
    | CST of (
# 12 "minic_parser.mly"
       (int)
# 38 "minic_parser.ml"
  )
    | COMMA
    | BOOL_CST of (
# 13 "minic_parser.mly"
       (bool)
# 44 "minic_parser.ml"
  )
    | BOOL
    | BEGIN
    | AND
    | ADD
  
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
  | MenhirState98
  | MenhirState95
  | MenhirState94
  | MenhirState90
  | MenhirState82
  | MenhirState79
  | MenhirState73
  | MenhirState71
  | MenhirState68
  | MenhirState63
  | MenhirState59
  | MenhirState58
  | MenhirState55
  | MenhirState53
  | MenhirState49
  | MenhirState48
  | MenhirState46
  | MenhirState38
  | MenhirState36
  | MenhirState34
  | MenhirState32
  | MenhirState30
  | MenhirState28
  | MenhirState26
  | MenhirState24
  | MenhirState22
  | MenhirState20
  | MenhirState18
  | MenhirState16
  | MenhirState11
  | MenhirState9
  | MenhirState8
  | MenhirState5
  | MenhirState0

# 1 "minic_parser.mly"
  
   open Lexing
   open Minic_ast

   let default_value = function
      | Int -> Cst(0)
      | Bool -> BCst(false)
	  | Void -> Unit()

# 111 "minic_parser.ml"

let rec _menhir_goto_function_decl : _menhir_env -> 'ttv_tail -> _menhir_state -> (Minic_ast.fun_def) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState98
    | INT ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState98
    | VOID ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState98
    | EOF ->
        _menhir_reduce3 _menhir_env (Obj.magic _menhir_stack) MenhirState98
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState98

and _menhir_goto_instruction : _menhir_env -> 'ttv_tail -> _menhir_state -> (Minic_ast.instr) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENT _v ->
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState82 _v
    | IF ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState82
    | PUTCHAR ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState82
    | RETURN ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState82
    | WHILE ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState82
    | END ->
        _menhir_reduce31 _menhir_env (Obj.magic _menhir_stack) MenhirState82
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState82

and _menhir_goto_arg_list : _menhir_env -> 'ttv_tail -> _menhir_state -> (Minic_ast.expr list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState38 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (e : (Minic_ast.expr))), _, (r : (Minic_ast.expr list))) = _menhir_stack in
        let _2 = () in
        let _v : (Minic_ast.expr list) = 
# 93 "minic_parser.mly"
                               ([e]@r)
# 169 "minic_parser.ml"
         in
        _menhir_goto_arg_list _menhir_env _menhir_stack _menhir_s _v
    | MenhirState11 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RPAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (f : (
# 14 "minic_parser.mly"
       (string)
# 184 "minic_parser.ml"
            ))), _, (p : (Minic_ast.expr list))) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _v : (Minic_ast.expr) = 
# 124 "minic_parser.mly"
                               (Call(f, p))
# 191 "minic_parser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_run16 : _menhir_env -> 'ttv_tail * _menhir_state * (Minic_ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL_CST _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState16 _v
    | CST _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState16 _v
    | IDENT _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState16 _v
    | NEG ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState16
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState16

and _menhir_run28 : _menhir_env -> 'ttv_tail * _menhir_state * (Minic_ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL_CST _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState28 _v
    | CST _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState28 _v
    | IDENT _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState28 _v
    | NEG ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState28
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState28

and _menhir_run18 : _menhir_env -> 'ttv_tail * _menhir_state * (Minic_ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL_CST _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState18 _v
    | CST _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState18 _v
    | IDENT _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState18 _v
    | NEG ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState18

and _menhir_run30 : _menhir_env -> 'ttv_tail * _menhir_state * (Minic_ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL_CST _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _v
    | CST _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _v
    | IDENT _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _v
    | NEG ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState30

and _menhir_run20 : _menhir_env -> 'ttv_tail * _menhir_state * (Minic_ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL_CST _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState20 _v
    | CST _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState20 _v
    | IDENT _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState20 _v
    | NEG ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState20
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState20

and _menhir_run32 : _menhir_env -> 'ttv_tail * _menhir_state * (Minic_ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL_CST _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState32 _v
    | CST _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState32 _v
    | IDENT _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState32 _v
    | NEG ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState32

and _menhir_run34 : _menhir_env -> 'ttv_tail * _menhir_state * (Minic_ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL_CST _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _v
    | CST _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _v
    | IDENT _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _v
    | NEG ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState34
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState34

and _menhir_run36 : _menhir_env -> 'ttv_tail * _menhir_state * (Minic_ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL_CST _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _v
    | CST _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _v
    | IDENT _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _v
    | NEG ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState36

and _menhir_run22 : _menhir_env -> 'ttv_tail * _menhir_state * (Minic_ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL_CST _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState22 _v
    | CST _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState22 _v
    | IDENT _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState22 _v
    | NEG ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState22
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState22

and _menhir_run26 : _menhir_env -> 'ttv_tail * _menhir_state * (Minic_ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL_CST _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _v
    | CST _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _v
    | IDENT _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _v
    | NEG ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState26
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState26

and _menhir_run24 : _menhir_env -> 'ttv_tail * _menhir_state * (Minic_ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL_CST _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState24 _v
    | CST _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState24 _v
    | IDENT _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState24 _v
    | NEG ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState24
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState24

and _menhir_goto_list_instruction_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Minic_ast.seq) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState71 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | END ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | ELSE ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | BEGIN ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    (match _tok with
                    | IDENT _v ->
                        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _v
                    | IF ->
                        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState79
                    | PUTCHAR ->
                        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState79
                    | RETURN ->
                        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState79
                    | WHILE ->
                        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState79
                    | END ->
                        _menhir_reduce31 _menhir_env (Obj.magic _menhir_stack) MenhirState79
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState79)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
            | END | IDENT _ | IF | PUTCHAR | RETURN | WHILE ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let (((_menhir_stack, _menhir_s), _, (e : (Minic_ast.expr))), _, (i1 : (Minic_ast.seq))) = _menhir_stack in
                let _7 = () in
                let _5 = () in
                let _4 = () in
                let _2 = () in
                let _1 = () in
                let _v : (Minic_ast.instr) = 
# 103 "minic_parser.mly"
                                                           (If(e,i1,[]))
# 458 "minic_parser.ml"
                 in
                _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState79 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | END ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((_menhir_stack, _menhir_s), _, (e : (Minic_ast.expr))), _, (i1 : (Minic_ast.seq))), _, (i2 : (Minic_ast.seq))) = _menhir_stack in
            let _11 = () in
            let _9 = () in
            let _8 = () in
            let _7 = () in
            let _5 = () in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : (Minic_ast.instr) = 
# 102 "minic_parser.mly"
                                                                                                (If(e,i1,i2))
# 494 "minic_parser.ml"
             in
            _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState82 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (x : (Minic_ast.instr))), _, (xs : (Minic_ast.seq))) = _menhir_stack in
        let _v : (Minic_ast.seq) = 
# 213 "<standard.mly>"
    ( x :: xs )
# 510 "minic_parser.ml"
         in
        _menhir_goto_list_instruction_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState58 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | END ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), _, (e : (Minic_ast.expr))), _, (i : (Minic_ast.seq))) = _menhir_stack in
            let _7 = () in
            let _5 = () in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : (Minic_ast.instr) = 
# 101 "minic_parser.mly"
                                                             (While(e,i))
# 531 "minic_parser.ml"
             in
            _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState53 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | END ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((((_menhir_stack, _menhir_s, (t : (Minic_ast.typ))), (f : (
# 14 "minic_parser.mly"
       (string)
# 552 "minic_parser.ml"
            ))), _), _, (v : ((string * Minic_ast.typ * Minic_ast.expr) list))), _, (s : (Minic_ast.seq))) = _menhir_stack in
            let _8 = () in
            let _5 = () in
            let _4 = () in
            let _3 = () in
            let _v : (Minic_ast.fun_def) = 
# 87 "minic_parser.mly"
( { name=f; code=s; params=[]; return=t; locals=v} )
# 561 "minic_parser.ml"
             in
            _menhir_goto_function_decl _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState95 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | END ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((((_menhir_stack, _menhir_s, (t : (Minic_ast.typ))), (f : (
# 14 "minic_parser.mly"
       (string)
# 582 "minic_parser.ml"
            ))), _, (p : ((string * Minic_ast.typ) list))), _, (v : ((string * Minic_ast.typ * Minic_ast.expr) list))), _, (s : (Minic_ast.seq))) = _menhir_stack in
            let _9 = () in
            let _6 = () in
            let _5 = () in
            let _3 = () in
            let _v : (Minic_ast.fun_def) = 
# 89 "minic_parser.mly"
( { name=f; code=s; params=p; return=t; locals=v} )
# 591 "minic_parser.ml"
             in
            _menhir_goto_function_decl _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_expression : _menhir_env -> 'ttv_tail -> _menhir_state -> (Minic_ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState38 | MenhirState11 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | COMMA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BOOL_CST _v ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _v
            | CST _v ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _v
            | IDENT _v ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _v
            | NEG ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState38
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState38)
        | DIV ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | INF ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | INFEQ ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | NEG ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | SUP ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | SUPEQ ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack)
        | RPAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (e : (Minic_ast.expr))) = _menhir_stack in
            let _v : (Minic_ast.expr list) = 
# 94 "minic_parser.mly"
              ([e])
# 657 "minic_parser.ml"
             in
            _menhir_goto_arg_list _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState16 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | COMMA | NEG | RPAR | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (Minic_ast.expr))), _, (e2 : (Minic_ast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Minic_ast.expr) = 
# 113 "minic_parser.mly"
                                    (SupEq(e1,e2))
# 688 "minic_parser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState18 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | COMMA | EQUAL | INF | INFEQ | NEG | RPAR | SEMI | SUP | SUPEQ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (Minic_ast.expr))), _, (e2 : (Minic_ast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Minic_ast.expr) = 
# 121 "minic_parser.mly"
                                 (Or(e1,e2))
# 715 "minic_parser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState20 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (e1 : (Minic_ast.expr))), _, (e2 : (Minic_ast.expr))) = _menhir_stack in
        let _2 = () in
        let _v : (Minic_ast.expr) = 
# 119 "minic_parser.mly"
                                  (Mul(e1,e2))
# 732 "minic_parser.ml"
         in
        _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
    | MenhirState22 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (e1 : (Minic_ast.expr))), _, (e2 : (Minic_ast.expr))) = _menhir_stack in
        let _2 = () in
        let _v : (Minic_ast.expr) = 
# 120 "minic_parser.mly"
                                  (Div(e1,e2))
# 743 "minic_parser.ml"
         in
        _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
    | MenhirState24 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (e1 : (Minic_ast.expr))), _, (e2 : (Minic_ast.expr))) = _menhir_stack in
        let _2 = () in
        let _v : (Minic_ast.expr) = 
# 116 "minic_parser.mly"
                                  (Add(e1,e2))
# 754 "minic_parser.ml"
         in
        _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
    | MenhirState26 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | COMMA | EQUAL | INF | INFEQ | NEG | RPAR | SEMI | SUP | SUPEQ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (Minic_ast.expr))), _, (e2 : (Minic_ast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Minic_ast.expr) = 
# 122 "minic_parser.mly"
                                  (And(e1,e2))
# 775 "minic_parser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState28 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | COMMA | NEG | RPAR | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (Minic_ast.expr))), _, (e2 : (Minic_ast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Minic_ast.expr) = 
# 112 "minic_parser.mly"
                                  (Sup(e1,e2))
# 806 "minic_parser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState30 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | INF ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | INFEQ ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | SUP ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | SUPEQ ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack)
        | COMMA | RPAR | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (Minic_ast.expr))), _, (e2 : (Minic_ast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Minic_ast.expr) = 
# 118 "minic_parser.mly"
                                  (Sub(e1,e2))
# 847 "minic_parser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState32 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | COMMA | NEG | RPAR | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (Minic_ast.expr))), _, (e2 : (Minic_ast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Minic_ast.expr) = 
# 115 "minic_parser.mly"
                                    (InfEq(e1,e2))
# 878 "minic_parser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState34 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | COMMA | NEG | RPAR | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (Minic_ast.expr))), _, (e2 : (Minic_ast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Minic_ast.expr) = 
# 114 "minic_parser.mly"
                                  (Inf(e1,e2))
# 909 "minic_parser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState36 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | COMMA | NEG | RPAR | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (Minic_ast.expr))), _, (e2 : (Minic_ast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Minic_ast.expr) = 
# 123 "minic_parser.mly"
                                    (Equal(e1,e2))
# 940 "minic_parser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState9 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | INF ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | INFEQ ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | SUP ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | SUPEQ ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack)
        | COMMA | RPAR | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e1 : (Minic_ast.expr))) = _menhir_stack in
            let _1 = () in
            let _v : (Minic_ast.expr) = 
# 117 "minic_parser.mly"
                    (Neg(e1))
# 981 "minic_parser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState8 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | INF ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | INFEQ ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | NEG ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (t : (Minic_ast.typ))), (x : (
# 14 "minic_parser.mly"
       (string)
# 1020 "minic_parser.ml"
            ))), _, (n : (Minic_ast.expr))) = _menhir_stack in
            let _5 = () in
            let _3 = () in
            let _v : (string * Minic_ast.typ * Minic_ast.expr) = 
# 60 "minic_parser.mly"
                                      ( (x, t, n) )
# 1027 "minic_parser.ml"
             in
            _menhir_goto_variable_decl _menhir_env _menhir_stack _menhir_s _v
        | SUP ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | SUPEQ ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState55 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | INF ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | INFEQ ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | NEG ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | RPAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BEGIN ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | IDENT _v ->
                    _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _v
                | IF ->
                    _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState58
                | PUTCHAR ->
                    _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState58
                | RETURN ->
                    _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState58
                | WHILE ->
                    _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState58
                | END ->
                    _menhir_reduce31 _menhir_env (Obj.magic _menhir_stack) MenhirState58
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState58)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | SUP ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | SUPEQ ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState59 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | INF ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | INFEQ ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | NEG ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e : (Minic_ast.expr))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (Minic_ast.instr) = 
# 99 "minic_parser.mly"
                           (Return(e))
# 1138 "minic_parser.ml"
             in
            _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v
        | SUP ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | SUPEQ ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState63 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | INF ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | INFEQ ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | NEG ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | RPAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | SEMI ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s), _, (e : (Minic_ast.expr))) = _menhir_stack in
                let _5 = () in
                let _4 = () in
                let _2 = () in
                let _1 = () in
                let _v : (Minic_ast.instr) = 
# 100 "minic_parser.mly"
                                      (Putchar(e))
# 1191 "minic_parser.ml"
                 in
                _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | SUP ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | SUPEQ ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState68 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | INF ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | INFEQ ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | NEG ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | RPAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BEGIN ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | IDENT _v ->
                    _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _v
                | IF ->
                    _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState71
                | PUTCHAR ->
                    _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState71
                | RETURN ->
                    _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState71
                | WHILE ->
                    _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState71
                | END ->
                    _menhir_reduce31 _menhir_env (Obj.magic _menhir_stack) MenhirState71
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState71)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | SUP ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | SUPEQ ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState73 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | INF ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | INFEQ ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | NEG ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (x : (
# 14 "minic_parser.mly"
       (string)
# 1305 "minic_parser.ml"
            ))), _, (e : (Minic_ast.expr))) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _v : (Minic_ast.instr) = 
# 104 "minic_parser.mly"
                                (Set(x,e))
# 1312 "minic_parser.ml"
             in
            _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v
        | SUP ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | SUPEQ ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_reduce31 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Minic_ast.seq) = 
# 211 "<standard.mly>"
    ( [] )
# 1333 "minic_parser.ml"
     in
    _menhir_goto_list_instruction_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run54 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAR ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BOOL_CST _v ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _v
        | CST _v ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _v
        | IDENT _v ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _v
        | NEG ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState55
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState55)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run59 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL_CST _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v
    | CST _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v
    | IDENT _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v
    | NEG ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState59

and _menhir_run62 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAR ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BOOL_CST _v ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
        | CST _v ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
        | IDENT _v ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
        | NEG ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState63)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run67 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAR ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BOOL_CST _v ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _v
        | CST _v ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _v
        | IDENT _v ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _v
        | NEG ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState68
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState68)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run72 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 14 "minic_parser.mly"
       (string)
# 1449 "minic_parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | SET ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BOOL_CST _v ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _v
        | CST _v ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _v
        | IDENT _v ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _v
        | NEG ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState73)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run9 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL_CST _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState9 _v
    | CST _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState9 _v
    | IDENT _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState9 _v
    | NEG ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState9
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState9

and _menhir_run10 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 14 "minic_parser.mly"
       (string)
# 1502 "minic_parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAR ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BOOL_CST _v ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState11 _v
        | CST _v ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState11 _v
        | IDENT _v ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState11 _v
        | NEG ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState11
        | RPAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState11 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (f : (
# 14 "minic_parser.mly"
       (string)
# 1530 "minic_parser.ml"
            ))) = _menhir_stack in
            let _3 = () in
            let _2 = () in
            let _v : (Minic_ast.expr) = 
# 125 "minic_parser.mly"
                    (Call(f, []))
# 1537 "minic_parser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState11)
    | ADD | AND | COMMA | DIV | EQUAL | INF | INFEQ | MUL | NEG | OR | RPAR | SEMI | SUP | SUPEQ ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, (x : (
# 14 "minic_parser.mly"
       (string)
# 1549 "minic_parser.ml"
        ))) = _menhir_stack in
        let _v : (Minic_ast.expr) = 
# 109 "minic_parser.mly"
          (Get(x))
# 1554 "minic_parser.ml"
         in
        _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run13 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 12 "minic_parser.mly"
       (int)
# 1567 "minic_parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (n : (
# 12 "minic_parser.mly"
       (int)
# 1575 "minic_parser.ml"
    )) = _v in
    let _v : (Minic_ast.expr) = 
# 110 "minic_parser.mly"
        ( Cst(n) )
# 1580 "minic_parser.ml"
     in
    _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v

and _menhir_run14 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 13 "minic_parser.mly"
       (bool)
# 1587 "minic_parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (b : (
# 13 "minic_parser.mly"
       (bool)
# 1595 "minic_parser.ml"
    )) = _v in
    let _v : (Minic_ast.expr) = 
# 111 "minic_parser.mly"
             ( BCst(b) )
# 1600 "minic_parser.ml"
     in
    _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_variable_decl : _menhir_env -> 'ttv_tail -> _menhir_state -> (string * Minic_ast.typ * Minic_ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState98 | MenhirState5 | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BOOL ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState5
        | INT ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState5
        | VOID ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState5
        | EOF ->
            _menhir_reduce3 _menhir_env (Obj.magic _menhir_stack) MenhirState5
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState5)
    | MenhirState94 | MenhirState49 | MenhirState48 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BOOL ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | INT ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | VOID ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | END | IDENT _ | IF | PUTCHAR | RETURN | WHILE ->
            _menhir_reduce33 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState49)
    | _ ->
        _menhir_fail ()

and _menhir_goto_list_variable_decl_ : _menhir_env -> 'ttv_tail -> _menhir_state -> ((string * Minic_ast.typ * Minic_ast.expr) list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState49 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (x : (string * Minic_ast.typ * Minic_ast.expr))), _, (xs : ((string * Minic_ast.typ * Minic_ast.expr) list))) = _menhir_stack in
        let _v : ((string * Minic_ast.typ * Minic_ast.expr) list) = 
# 213 "<standard.mly>"
    ( x :: xs )
# 1656 "minic_parser.ml"
         in
        _menhir_goto_list_variable_decl_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState48 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | IDENT _v ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _v
        | IF ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | PUTCHAR ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | RETURN ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | WHILE ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | END ->
            _menhir_reduce31 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState53)
    | MenhirState94 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | IDENT _v ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState95 _v
        | IF ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | PUTCHAR ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | RETURN ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | WHILE ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | END ->
            _menhir_reduce31 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState95)
    | _ ->
        _menhir_fail ()

and _menhir_fail : unit -> 'a =
  fun () ->
    Printf.fprintf stderr "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

and _menhir_goto_parameter_list : _menhir_env -> 'ttv_tail -> _menhir_state -> ((string * Minic_ast.typ) list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState90 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s, (t : (Minic_ast.typ))), (x : (
# 14 "minic_parser.mly"
       (string)
# 1719 "minic_parser.ml"
        ))), _, (r : ((string * Minic_ast.typ) list))) = _menhir_stack in
        let _3 = () in
        let _v : ((string * Minic_ast.typ) list) = 
# 66 "minic_parser.mly"
                                       ([(x, t)]@r)
# 1725 "minic_parser.ml"
         in
        _menhir_goto_parameter_list _menhir_env _menhir_stack _menhir_s _v
    | MenhirState46 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RPAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BEGIN ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | BOOL ->
                    _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState94
                | INT ->
                    _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState94
                | VOID ->
                    _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState94
                | END | IDENT _ | IF | PUTCHAR | RETURN | WHILE ->
                    _menhir_reduce33 _menhir_env (Obj.magic _menhir_stack) MenhirState94
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState94)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_run8 : _menhir_env -> ('ttv_tail * _menhir_state * (Minic_ast.typ)) * (
# 14 "minic_parser.mly"
       (string)
# 1773 "minic_parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL_CST _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState8 _v
    | CST _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState8 _v
    | IDENT _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState8 _v
    | NEG ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState8
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState8

and _menhir_run45 : _menhir_env -> ('ttv_tail * _menhir_state * (Minic_ast.typ)) * (
# 14 "minic_parser.mly"
       (string)
# 1795 "minic_parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let ((_menhir_stack, _menhir_s, (t : (Minic_ast.typ))), (x : (
# 14 "minic_parser.mly"
       (string)
# 1803 "minic_parser.ml"
    ))) = _menhir_stack in
    let _3 = () in
    let _v : (string * Minic_ast.typ * Minic_ast.expr) = 
# 61 "minic_parser.mly"
                     ( (x,t,default_value t) )
# 1809 "minic_parser.ml"
     in
    _menhir_goto_variable_decl _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce33 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : ((string * Minic_ast.typ * Minic_ast.expr) list) = 
# 211 "<standard.mly>"
    ( [] )
# 1818 "minic_parser.ml"
     in
    _menhir_goto_list_variable_decl_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_program : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 30 "minic_parser.mly"
      (Minic_ast.prog)
# 1825 "minic_parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_1 : (
# 30 "minic_parser.mly"
      (Minic_ast.prog)
# 1833 "minic_parser.ml"
    )) = _v in
    Obj.magic _1

and _menhir_goto_declaration_list : _menhir_env -> 'ttv_tail -> _menhir_state -> ((string * Minic_ast.typ * Minic_ast.expr) list * Minic_ast.fun_def list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState98 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (fd : (Minic_ast.fun_def))), _, (dl : ((string * Minic_ast.typ * Minic_ast.expr) list * Minic_ast.fun_def list))) = _menhir_stack in
        let _v : ((string * Minic_ast.typ * Minic_ast.expr) list * Minic_ast.fun_def list) = 
# 54 "minic_parser.mly"
                                       ( let vl, fl = dl in vl, fd :: fl )
# 1848 "minic_parser.ml"
         in
        _menhir_goto_declaration_list _menhir_env _menhir_stack _menhir_s _v
    | MenhirState5 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (vd : (string * Minic_ast.typ * Minic_ast.expr))), _, (dl : ((string * Minic_ast.typ * Minic_ast.expr) list * Minic_ast.fun_def list))) = _menhir_stack in
        let _v : ((string * Minic_ast.typ * Minic_ast.expr) list * Minic_ast.fun_def list) = 
# 53 "minic_parser.mly"
                                       ( let vl, fl = dl in vd :: vl, fl )
# 1858 "minic_parser.ml"
         in
        _menhir_goto_declaration_list _menhir_env _menhir_stack _menhir_s _v
    | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | EOF ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (dl : ((string * Minic_ast.typ * Minic_ast.expr) list * Minic_ast.fun_def list))) = _menhir_stack in
            let _2 = () in
            let _v : (
# 30 "minic_parser.mly"
      (Minic_ast.prog)
# 1874 "minic_parser.ml"
            ) = 
# 39 "minic_parser.mly"
       ( let var_list, fun_list = dl in
         { globals = var_list; functions = fun_list; } )
# 1879 "minic_parser.ml"
             in
            _menhir_goto_program _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_typ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Minic_ast.typ) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState0 | MenhirState98 | MenhirState5 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | IDENT _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = (_menhir_stack, _v) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | LPAR ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | BOOL ->
                    _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState46
                | INT ->
                    _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState46
                | RPAR ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_s = MenhirState46 in
                    let _menhir_stack = (_menhir_stack, _menhir_s) in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    (match _tok with
                    | BEGIN ->
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let _menhir_env = _menhir_discard _menhir_env in
                        let _tok = _menhir_env._menhir_token in
                        (match _tok with
                        | BOOL ->
                            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState48
                        | INT ->
                            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState48
                        | VOID ->
                            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState48
                        | END | IDENT _ | IF | PUTCHAR | RETURN | WHILE ->
                            _menhir_reduce33 _menhir_env (Obj.magic _menhir_stack) MenhirState48
                        | _ ->
                            assert (not _menhir_env._menhir_error);
                            _menhir_env._menhir_error <- true;
                            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState48)
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let (_menhir_stack, _menhir_s) = _menhir_stack in
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
                | VOID ->
                    _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState46
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState46)
            | SEMI ->
                _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
            | SET ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState94 | MenhirState48 | MenhirState49 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | IDENT _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = (_menhir_stack, _v) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | SEMI ->
                _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
            | SET ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState90 | MenhirState46 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | IDENT _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = (_menhir_stack, _v) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | COMMA ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | BOOL ->
                    _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState90
                | INT ->
                    _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState90
                | VOID ->
                    _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState90
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState90)
            | RPAR ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s, (t : (Minic_ast.typ))), (x : (
# 14 "minic_parser.mly"
       (string)
# 2025 "minic_parser.ml"
                ))) = _menhir_stack in
                let _v : ((string * Minic_ast.typ) list) = 
# 67 "minic_parser.mly"
                ([(x, t)])
# 2030 "minic_parser.ml"
                 in
                _menhir_goto_parameter_list _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState98 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState95 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState94 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState90 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState82 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState79 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState73 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState71 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState68 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState63 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState59 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState58 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState55 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState53 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState49 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState48 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState46 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState38 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState36 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState34 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState32 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState30 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState28 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState26 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState24 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState22 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState20 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState18 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState16 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState11 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState9 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState8 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState5 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState0 in
        let _startpos = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _startpos__1_ = _startpos in
        let _1 = () in
        let _v : (
# 30 "minic_parser.mly"
      (Minic_ast.prog)
# 2194 "minic_parser.ml"
        ) = let _startpos = _startpos__1_ in
        
# 41 "minic_parser.mly"
        ( let pos = _startpos in
          let message =
            Printf.sprintf
              "Syntax error at %d, %d"
              pos.pos_lnum (pos.pos_cnum - pos.pos_bol)
          in
          failwith message )
# 2205 "minic_parser.ml"
         in
        _menhir_goto_program _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce3 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : ((string * Minic_ast.typ * Minic_ast.expr) list * Minic_ast.fun_def list) = 
# 52 "minic_parser.mly"
  ( [], [] )
# 2214 "minic_parser.ml"
     in
    _menhir_goto_declaration_list _menhir_env _menhir_stack _menhir_s _v

and _menhir_run2 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Minic_ast.typ) = 
# 74 "minic_parser.mly"
       ( Void )
# 2226 "minic_parser.ml"
     in
    _menhir_goto_typ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run3 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Minic_ast.typ) = 
# 72 "minic_parser.mly"
      ( Int )
# 2238 "minic_parser.ml"
     in
    _menhir_goto_typ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run4 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Minic_ast.typ) = 
# 73 "minic_parser.mly"
       ( Bool )
# 2250 "minic_parser.ml"
     in
    _menhir_goto_typ _menhir_env _menhir_stack _menhir_s _v

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

and program : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (
# 30 "minic_parser.mly"
      (Minic_ast.prog)
# 2269 "minic_parser.ml"
) =
  fun lexer lexbuf ->
    let _menhir_env = let _tok = Obj.magic () in
    {
      _menhir_lexer = lexer;
      _menhir_lexbuf = lexbuf;
      _menhir_token = _tok;
      _menhir_error = false;
    } in
    Obj.magic (let _menhir_stack = ((), _menhir_env._menhir_lexbuf.Lexing.lex_curr_p) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | INT ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | VOID ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | EOF ->
        _menhir_reduce3 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState0)

# 269 "<standard.mly>"
  

# 2299 "minic_parser.ml"
