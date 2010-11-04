module Make
          (Semantics : sig
  type number
  val inject: int -> number
  val ( + ): number -> number -> number
  val ( - ): number -> number -> number
  val ( * ): number -> number -> number
  val ( / ): number -> number -> number
  val ( ~-): number -> number
end)
= struct

  exception Error
  
  type _menhir_env = {
    _menhir_lexer: Lexing.lexbuf -> Tokens.token;
    _menhir_lexbuf: Lexing.lexbuf;
    mutable _menhir_token: Tokens.token;
    mutable _menhir_startp: Lexing.position;
    mutable _menhir_endp: Lexing.position;
    mutable _menhir_shifted: int
  }
  
  and _menhir_state = 
    | MenhirState12
    | MenhirState10
    | MenhirState8
    | MenhirState5
    | MenhirState2
    | MenhirState1
    | MenhirState0
  
    

  open Semantics

let _eRR =
    Error
  
  let rec _menhir_run5 : _menhir_env -> 'ttv_tail * _menhir_state * (Semantics.number) -> 'ttv_return =
    fun _menhir_env _menhir_stack ->
      let _tok = _menhir_discard _menhir_env in
      match _tok with
      | Tokens.INT _v ->
          _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState5 _v
      | Tokens.LPAREN ->
          _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState5
      | Tokens.MINUS ->
          _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState5
      | _ ->
          assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
          _menhir_env._menhir_shifted <- (-1);
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState5
  
  and _menhir_run8 : _menhir_env -> 'ttv_tail * _menhir_state * (Semantics.number) -> 'ttv_return =
    fun _menhir_env _menhir_stack ->
      let _tok = _menhir_discard _menhir_env in
      match _tok with
      | Tokens.INT _v ->
          _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState8 _v
      | Tokens.LPAREN ->
          _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState8
      | Tokens.MINUS ->
          _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState8
      | _ ->
          assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
          _menhir_env._menhir_shifted <- (-1);
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState8
  
  and _menhir_run12 : _menhir_env -> 'ttv_tail * _menhir_state * (Semantics.number) -> 'ttv_return =
    fun _menhir_env _menhir_stack ->
      let _tok = _menhir_discard _menhir_env in
      match _tok with
      | Tokens.INT _v ->
          _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _v
      | Tokens.LPAREN ->
          _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState12
      | Tokens.MINUS ->
          _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState12
      | _ ->
          assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
          _menhir_env._menhir_shifted <- (-1);
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState12
  
  and _menhir_run10 : _menhir_env -> 'ttv_tail * _menhir_state * (Semantics.number) -> 'ttv_return =
    fun _menhir_env _menhir_stack ->
      let _tok = _menhir_discard _menhir_env in
      match _tok with
      | Tokens.INT _v ->
          _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState10 _v
      | Tokens.LPAREN ->
          _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState10
      | Tokens.MINUS ->
          _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState10
      | _ ->
          assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
          _menhir_env._menhir_shifted <- (-1);
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState10
  
  and _menhir_goto_expr : _menhir_env -> 'ttv_tail -> _menhir_state -> (Semantics.number) -> 'ttv_return =
    fun _menhir_env _menhir_stack _menhir_s _v ->
      let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
      match _menhir_s with
      | MenhirState2 ->
          let _menhir_stack = Obj.magic _menhir_stack in
          assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | Tokens.DIV ->
              _menhir_run10 _menhir_env (Obj.magic _menhir_stack)
          | Tokens.MINUS ->
              _menhir_run12 _menhir_env (Obj.magic _menhir_stack)
          | Tokens.PLUS ->
              _menhir_run8 _menhir_env (Obj.magic _menhir_stack)
          | Tokens.RPAREN ->
              let _menhir_stack = Obj.magic _menhir_stack in
              let _ = _menhir_discard _menhir_env in
              let _menhir_stack = Obj.magic _menhir_stack in
              let ((_menhir_stack, _menhir_s), _, e) = _menhir_stack in
              let _v : (Semantics.number) =     ( e ) in
              _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
          | Tokens.TIMES ->
              _menhir_run5 _menhir_env (Obj.magic _menhir_stack)
          | _ ->
              assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
              _menhir_env._menhir_shifted <- (-1);
              let _menhir_stack = Obj.magic _menhir_stack in
              let (_menhir_stack, _menhir_s, _) = _menhir_stack in
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
      | MenhirState5 ->
          let _menhir_stack = Obj.magic _menhir_stack in
          let _menhir_stack = Obj.magic _menhir_stack in
          let ((_menhir_stack, _menhir_s, e1), _, e2) = _menhir_stack in
          let _v : (Semantics.number) =     ( e1 * e2 ) in
          _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
      | MenhirState8 ->
          let _menhir_stack = Obj.magic _menhir_stack in
          assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | Tokens.DIV ->
              _menhir_run10 _menhir_env (Obj.magic _menhir_stack)
          | Tokens.TIMES ->
              _menhir_run5 _menhir_env (Obj.magic _menhir_stack)
          | Tokens.EOL | Tokens.MINUS | Tokens.PLUS | Tokens.RPAREN ->
              let _menhir_stack = Obj.magic _menhir_stack in
              let ((_menhir_stack, _menhir_s, e1), _, e2) = _menhir_stack in
              let _v : (Semantics.number) =     ( e1 + e2 ) in
              _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
          | _ ->
              assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
              _menhir_env._menhir_shifted <- (-1);
              let _menhir_stack = Obj.magic _menhir_stack in
              let (_menhir_stack, _menhir_s, _) = _menhir_stack in
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
      | MenhirState10 ->
          let _menhir_stack = Obj.magic _menhir_stack in
          let _menhir_stack = Obj.magic _menhir_stack in
          let ((_menhir_stack, _menhir_s, e1), _, e2) = _menhir_stack in
          let _v : (Semantics.number) =     ( e1 / e2 ) in
          _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
      | MenhirState12 ->
          let _menhir_stack = Obj.magic _menhir_stack in
          assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | Tokens.DIV ->
              _menhir_run10 _menhir_env (Obj.magic _menhir_stack)
          | Tokens.TIMES ->
              _menhir_run5 _menhir_env (Obj.magic _menhir_stack)
          | Tokens.EOL | Tokens.MINUS | Tokens.PLUS | Tokens.RPAREN ->
              let _menhir_stack = Obj.magic _menhir_stack in
              let ((_menhir_stack, _menhir_s, e1), _, e2) = _menhir_stack in
              let _v : (Semantics.number) =     ( e1 - e2 ) in
              _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
          | _ ->
              assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
              _menhir_env._menhir_shifted <- (-1);
              let _menhir_stack = Obj.magic _menhir_stack in
              let (_menhir_stack, _menhir_s, _) = _menhir_stack in
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
      | MenhirState1 ->
          let _menhir_stack = Obj.magic _menhir_stack in
          let _menhir_stack = Obj.magic _menhir_stack in
          let ((_menhir_stack, _menhir_s), _, e) = _menhir_stack in
          let _v : (Semantics.number) =     ( - e ) in
          _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
      | MenhirState0 ->
          let _menhir_stack = Obj.magic _menhir_stack in
          assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
          let _tok = _menhir_env._menhir_token in
          match _tok with
          | Tokens.DIV ->
              _menhir_run10 _menhir_env (Obj.magic _menhir_stack)
          | Tokens.EOL ->
              let _menhir_stack = Obj.magic _menhir_stack in
              let _menhir_stack = Obj.magic _menhir_stack in
              let (_menhir_stack, _menhir_s, e) = _menhir_stack in
              let _v : (Semantics.number) =     ( e ) in
              let _menhir_stack = Obj.magic _menhir_stack in
              let _menhir_stack = Obj.magic _menhir_stack in
              let _1 = _v in
              Obj.magic _1
          | Tokens.MINUS ->
              _menhir_run12 _menhir_env (Obj.magic _menhir_stack)
          | Tokens.PLUS ->
              _menhir_run8 _menhir_env (Obj.magic _menhir_stack)
          | Tokens.TIMES ->
              _menhir_run5 _menhir_env (Obj.magic _menhir_stack)
          | _ ->
              assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
              _menhir_env._menhir_shifted <- (-1);
              let _menhir_stack = Obj.magic _menhir_stack in
              let (_menhir_stack, _menhir_s, _) = _menhir_stack in
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
  
  and _menhir_discard : _menhir_env -> Tokens.token =
    fun _menhir_env ->
      let lexbuf = _menhir_env._menhir_lexbuf in
      let _tok = _menhir_env._menhir_lexer lexbuf in
      _menhir_env._menhir_token <- _tok;
      _menhir_env._menhir_startp <- lexbuf.Lexing.lex_start_p;
      _menhir_env._menhir_endp <- lexbuf.Lexing.lex_curr_p;
      let shifted = Pervasives.(+) _menhir_env._menhir_shifted 1 in
      if Pervasives.(>=) shifted 0 then
        _menhir_env._menhir_shifted <- shifted;
      _tok
  
  and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
    fun _menhir_env _menhir_stack _menhir_s ->
      match _menhir_s with
      | MenhirState12 ->
          let _menhir_stack = Obj.magic _menhir_stack in
          let (_menhir_stack, _menhir_s, _) = _menhir_stack in
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
      | MenhirState10 ->
          let _menhir_stack = Obj.magic _menhir_stack in
          let (_menhir_stack, _menhir_s, _) = _menhir_stack in
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
      | MenhirState8 ->
          let _menhir_stack = Obj.magic _menhir_stack in
          let (_menhir_stack, _menhir_s, _) = _menhir_stack in
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
      | MenhirState5 ->
          let _menhir_stack = Obj.magic _menhir_stack in
          let (_menhir_stack, _menhir_s, _) = _menhir_stack in
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
      | MenhirState2 ->
          let _menhir_stack = Obj.magic _menhir_stack in
          let (_menhir_stack, _menhir_s) = _menhir_stack in
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
      | MenhirState1 ->
          let _menhir_stack = Obj.magic _menhir_stack in
          let (_menhir_stack, _menhir_s) = _menhir_stack in
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
      | MenhirState0 ->
          let _menhir_stack = Obj.magic _menhir_stack in
          raise _eRR
  
  and _menhir_run1 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
    fun _menhir_env _menhir_stack _menhir_s ->
      let _menhir_stack = (_menhir_stack, _menhir_s) in
      let _tok = _menhir_discard _menhir_env in
      match _tok with
      | Tokens.INT _v ->
          _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState1 _v
      | Tokens.LPAREN ->
          _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState1
      | Tokens.MINUS ->
          _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState1
      | _ ->
          assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
          _menhir_env._menhir_shifted <- (-1);
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState1
  
  and _menhir_run2 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
    fun _menhir_env _menhir_stack _menhir_s ->
      let _menhir_stack = (_menhir_stack, _menhir_s) in
      let _tok = _menhir_discard _menhir_env in
      match _tok with
      | Tokens.INT _v ->
          _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState2 _v
      | Tokens.LPAREN ->
          _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState2
      | Tokens.MINUS ->
          _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState2
      | _ ->
          assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
          _menhir_env._menhir_shifted <- (-1);
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState2
  
  and _menhir_run3 : _menhir_env -> 'ttv_tail -> _menhir_state -> (int) -> 'ttv_return =
    fun _menhir_env _menhir_stack _menhir_s _v ->
      let _ = _menhir_discard _menhir_env in
      let _menhir_stack = Obj.magic _menhir_stack in
      let i = _v in
      let _v : (Semantics.number) =     ( inject i ) in
      _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
  
  and main : (Lexing.lexbuf -> Tokens.token) -> Lexing.lexbuf -> (Semantics.number) =
    fun lexer lexbuf ->
      let _menhir_env = let _tok = lexer lexbuf in
      {
        _menhir_lexer = lexer;
        _menhir_lexbuf = lexbuf;
        _menhir_token = _tok;
        _menhir_startp = lexbuf.Lexing.lex_start_p;
        _menhir_endp = lexbuf.Lexing.lex_curr_p;
        _menhir_shifted = 4611686018427387903;
        } in
      Obj.magic (let _menhir_stack = () in
      assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
      let _tok = _menhir_env._menhir_token in
      match _tok with
      | Tokens.INT _v ->
          _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
      | Tokens.LPAREN ->
          _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState0
      | Tokens.MINUS ->
          _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState0
      | _ ->
          assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
          _menhir_env._menhir_shifted <- (-1);
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState0)
  
  



end
