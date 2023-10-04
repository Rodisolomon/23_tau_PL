structure Eval : sig

  val isV  : AST.term -> bool
  val isNV : AST.term -> bool
  val step : AST.term -> AST.term option
  val eval : AST.term -> AST.term list
				  
end = struct

  structure A = AST

  fun isNV _ = raise Fail "todo"

  fun isV _ = raise Fail "todo"
		 
  fun step _ = raise Fail "todo"

  fun eval _ = raise Fail "todo"
	 
end
