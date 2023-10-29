structure TypeEnv : sig

  type env

  val empty  : env
  val lookup : env * string -> Type.typ option
  val extend : env * string * Type.typ -> env
	    
end = struct

  type env = unit (* todo: replace this *)
	       
  val empty = () (* todo: replace this *)

  fun lookup _ = raise Fail "todo"
  fun extend _ = raise Fail "todo"
					  
end
