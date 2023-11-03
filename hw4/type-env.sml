structure TypeEnv :> sig

  type env

  val empty  : env
  val lookup : env * string -> Type.typ option
  val extend : env * string * Type.typ -> env
	    
end = struct
  (* todo: two list must have same length *)
  type env = (string * Type.typ) list 
	       
  val empty = [] (* as var-set, still use a list *)

  fun lookup (env, key) = 
    case List.find (fn (k, _) => k = key) env of
         NONE => NONE
       | SOME (_, typ) => SOME typ


  fun extend (env, new_key, new_type) = (new_key, new_type) :: env
					  
end
