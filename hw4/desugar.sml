structure Desugar : sig

  val desugar : Sugary.term -> ULC.term

end = struct

  structure S = Sugary
  structure U = ULC

  fun desugar _ = raise Fail "todo"

end
