structure VarSet :> sig

  type set

  val empty  : set
  val mem    : string * set -> bool
  val ins    : string * set -> set
  val rem    : string * set -> set
  val union  : set * set -> set

(* the following operations aren't necessary for substitution *)
(* it just seemed sad not to have them for sets *)
(* they also make testing much easier *)

  val sing   : string -> set (* singleton set *)
  val size   : set -> int
  val subset : set * set -> bool
  val equals : set * set -> bool
			      
  val toList : set -> string list

end = struct

  type set = unit (* todo: replace this *)
  val empty = ()  (* todo: replace this *)

  fun mem _    = raise Fail "todo"
  fun ins _    = raise Fail "todo"
  fun rem _    = raise Fail "todo"
  fun union _  = raise Fail "todo"
  fun sing _   = raise Fail "todo"
  fun size _   = raise Fail "todo"
  fun subset _ = raise Fail "todo"
  fun equals _ = raise Fail "todo"
  fun toList _ = raise Fail "todo"
      				  
end
