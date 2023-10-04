structure Compile : sig

  val code : string -> AST.term list
  val file : string -> AST.term list
			 
end = struct
 
  fun code program =
    let
      val tokens = Scan.scan program
      val ast    = Parse.parse tokens
      val terms  = Eval.eval ast
    in
      terms
    end

  fun file filename =
    let
      val program = ReadFile.toString filename
    in
      code program
    end

end
