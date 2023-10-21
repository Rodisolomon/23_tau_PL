structure VarSet :> sig

  type set

  val empty : set
  val mem   : string * set -> bool
  val ins   : string * set -> set
  val rem   : string * set -> set
  val union : set * set -> set

end = struct

  type set = int (* <== Change this to something else! *)

  val empty = 0  (* <== Change this to something consistent with the new set type. *)

  fun mem _ = raise Fail "todo: VarSet.mem"

  fun ins _ = raise Fail "todo: VarSet.ins"

  fun rem _ = raise Fail "todo: VarSet.rem"

  fun union _ = raise Fail "todo: VarSet.union"
				      
end
