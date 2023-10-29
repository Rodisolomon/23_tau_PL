structure FullBeta : sig

  val step : ULC.term -> ULC.term option

end = struct

  structure U = ULC

  fun step _ = raise Fail "todo"

end
