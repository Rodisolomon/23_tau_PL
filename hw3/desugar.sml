structure Desugar : sig

  val desugar : Sweetl.term -> ULC.term

end = struct

  fun desugar _ = raise Fail "todo: Desugar.desugar"

end
