structure Compile : sig

  val code : string -> Eval.norm * Type.typ
  val file : string -> Eval.norm * Type.typ
			 
end = struct
 
  fun code program =
    let
      val tokens  = Scan.scan program
      val sweet   = Parse.parse tokens
      val tau     = TypeCheck.typeof sweet
      val unsweet = Desugar.desugar sweet
      val result  = Eval.result unsweet
    in
      (result, tau)
    end

  fun file filename =
    let
      val program = ReadFile.toString filename
    in
      code program
    end

end
