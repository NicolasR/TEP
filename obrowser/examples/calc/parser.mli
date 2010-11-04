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
: sig

  exception Error
  
  
  val main: (Lexing.lexbuf -> Tokens.token) -> Lexing.lexbuf -> (Semantics.number)

end
