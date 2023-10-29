structure TypeCheck : sig
	    
  val typeof : TypeEnv.env * Sugary.term -> Type.typ
	    
end = struct

  structure S = Sugary
  structure T = Type

  fun typeof _ = raise Fail "todo"

end
