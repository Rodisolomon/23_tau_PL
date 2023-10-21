structure Subst : sig

  val fv : ULC.term -> VarSet.set
  val subst : string * ULC.term * ULC.term -> ULC.term

end = struct
		  
  fun fv _ = raise Fail "todo: Subst.fv"
		  
  fun subst _ = raise Fail "todo: Subst.subst"

end
