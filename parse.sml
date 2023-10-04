structure Parse : sig

  val next  : Token.token list -> (AST.term * Token.token list) option
  val parse : Token.token list -> AST.term

end = struct

  structure T = Token
  structure A = AST
  
  fun next _ = raise Fail "todo"

  fun parse _ = raise Fail "todo"
		     
end
