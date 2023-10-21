structure CBV : sig

  val step : ULC.term -> ULC.term option

end = struct

  fun step _ = raise Fail "todo: CBV.step"

end
