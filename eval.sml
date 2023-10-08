structure Eval : sig

  val isV  : AST.term -> bool
  val isNV : AST.term -> bool
  val step : AST.term -> AST.term option
  val eval : AST.term -> AST.term list
				  
end = struct

  structure A = AST

  fun isNV (AST.Zero) = true
    | isNV (AST.Succ t) = isNV t
    | isNV _ = false;


  fun isV x = 
		(x=A.True) orelse (x=A.False) orelse (isNV x);

  fun step _ = raise Fail "todo"

  fun eval _ = raise Fail "todo"
	 
end
