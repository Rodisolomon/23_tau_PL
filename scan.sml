structure Scan : sig

  val next : char list -> (Token.token * char list) option
  val scan : string -> Token.token list
	    
end = struct

  structure T = Token

  fun next _ = raise Fail "todo"

  fun scan _ = raise Fail "todo"
      
end
