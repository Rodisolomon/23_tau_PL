structure Lazy : sig

  val step : ULC.term -> ULC.term option

end = struct
		  
  fun step _ = raise Fail "todo: Lazy.step"		   

end
