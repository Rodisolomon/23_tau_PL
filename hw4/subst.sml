structure Subst : sig

  val fv : ULC.term -> VarSet.set
  val subst : string * ULC.term * ULC.term -> ULC.term

end = struct

  structure U = ULC
  structure S = VarSet
		  
  fun fv _ = raise Fail "todo"
		   
  fun subst _ = raise Fail "todo"

end
