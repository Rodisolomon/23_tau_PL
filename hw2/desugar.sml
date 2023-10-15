structure Desugar : sig

  val desugar : Sugary.term -> Desugared.term

end = struct

  structure S = Sugary
  structure D = Desugared

  fun desugar _ = raise Fail "todo"
      
end
