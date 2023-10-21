structure Test : sig

  val all : unit -> unit
	    
end = struct

  fun println s = TextIO.print (s ^ "\n")

  (* some renamings for convenience... *)
  val lam = ULC.Lam
  val v   = ULC.Var
			       
  fun all () =
    let
      val _ = Check.expect (Compile.cbv "([x x] [y y])",
			    lam ("y", v "y"),
			    "test0")
      val _ = Check.expect (Compile.cbv "(&x &y)",
			    lam ("y", v "y"),
			    "test1")
      val _ = Check.expect (Compile.cbv ":idx=&x; (:idx &y)",
			    lam ("y", v "y"),
			    "test2")
      (* tests here *)
    in
      println "== tests complete"
    end

end
