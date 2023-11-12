structure Eval : sig

  val eval : L23RR.term -> L23RR.term
	    
end = struct

  structure L = L23RR

  fun eval _ = raise Fail "todo: Eval.eval"
		 
end
