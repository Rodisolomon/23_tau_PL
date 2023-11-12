structure RecordCheck : sig

(* check for pairwise distinct labels at all levels of record expressions and record types *)
(* also, reject empty records if you encounter them *)

(* raise an exception if the term doesn't pass the check *)

(* otherwise, return the term as is *)

  val check : L23RR.term -> L23RR.term
	    
end = struct

  structure L = L23RR

  fun check _ = raise Fail "todo: RecordCheck.check"

end
