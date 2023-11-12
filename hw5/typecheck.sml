structure TypeCheck : sig

(* return true if the first type is a subtype of the second *)
  val subty : Type.typ * Type.typ -> bool

(* for commonSupertype, use the s function from the PDF *)
(* if there isn't a common supertype, return NONE *)
  val commonSupertype : Type.typ * Type.typ -> Type.typ option

  val typeof : L23RR.term -> Type.typ
							
end = struct

  structure L = L23RR
  structure T = Type
  structure E = TypeEnv
		  
  fun subty _ = raise Fail "todo: TypeCheck.subty"

  fun commonSupertype _ = raise Fail "todo: TypeCheck.commonSupertype"

  fun typeof _ = raise Fail "todo: TypeCheck.typeof"
	    
end
