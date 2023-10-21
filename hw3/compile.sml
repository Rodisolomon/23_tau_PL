structure Compile : sig

  val result   : (ULC.term -> ULC.term option) -> string -> ULC.term
  val cbv      : string -> ULC.term
  val lazy     : string -> ULC.term
  val fullBeta : string -> ULC.term

  (* these consume file names rather than code *)
  val resultFile   : (ULC.term -> ULC.term option) -> string -> ULC.term
  val cbvFile      : string -> ULC.term
  val lazyFile     : string -> ULC.term
  val fullBetaFile : string -> ULC.term
	    
end = struct

  structure S = Sweetl
  structure U = ULC

  fun println s = TextIO.print (s ^ "\n")

  fun result step code =
    let
      val toks = Scan.scan code
      val prog = Parse.parse toks
      val _    = println ("==> new evaluation...")   
      val _    = (println "--- Sweetl source code:";
		  println (S.tos prog);
		  println "")
      val ast  = Unroll.unroll prog
      val _    = (println "--- after unrolling";
		  println (S.tos (S.Prog ([], ast)));
		  println "")
      val ulc  = Desugar.desugar ast
      val _    = (println "--- after desugaring";
		  println (U.tos ulc);
		  println "")
      val _    = println "--- evaluation"
      fun loop t =
        let
	  val _ = println (U.tos t)
	in
	  case step t
            of SOME t' => loop t'
	     | NONE => (println ""; t)
	end
    in
      loop ulc
    end
  val cbv      = result CBV.step
  val lazy     = result Lazy.step
  val fullBeta = result FullBeta.step

  fun resultFile step filename =
    let
      val code = ReadFile.toString filename
    in
      result step code
    end      
  val cbvFile      = resultFile CBV.step
  val lazyFile     = resultFile Lazy.step
  val fullBetaFile = resultFile FullBeta.step

end
