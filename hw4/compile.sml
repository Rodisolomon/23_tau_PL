structure Compile : sig

  datatype result
    = Value of string * Type.typ
    | TypeError of string
	
  val code : string -> result
  val file : string -> result
			 
end = struct

  datatype result
    = Value of string * Type.typ
    | TypeError of string

  fun eval t =
    case FullBeta.step t
      of NONE => t
       | SOME t' => eval t'

  fun code program =
    let
      val tokens  = Scan.scan program
      val sweet   = Parse.parse tokens
    in
     (case TypeCheck.typeof (TypeEnv.empty, sweet)
        of tau => let
	            val unsweet = Desugar.desugar sweet
		    val norm    = eval unsweet
	          in
	            Value (ULC.tos norm, tau)
	end)
      handle Fail msg => TypeError (if msg="" then "?" else msg)
	   | _ => TypeError "?"
    end

  fun file filename =
    let
      val program = ReadFile.toString filename
    in
      code program
    end

end
