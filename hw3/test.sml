structure Test : sig

  val all : unit -> unit
	    
end = struct

  structure U = Unroll
  structure D = Desugar
  structure S = Sweetl
  fun println s = TextIO.print (s ^ "\n")

  (* some renamings for convenience... *)
  val lam = ULC.Lam
  val v   = ULC.Var


  fun unroll () =
    let
      val _ = Check.expect(U.unroll(
        S.Prog(
            [("ab", S.Lam("a", S.Var("b"))), ("cd", S.Lam("c", S.Var("d")))], 
            S.App(S.Abbr("ab"), S.Abbr("cd"))
          )
        ), 
        S.App( S.Lam("a", S.Var("b")), S.Lam("c", S.Var("d")) ),
        "unroll 1")
      val _ = Check.expect(U.unroll(
        S.Prog(
            [("ab", S.Lam("a", S.Var("b"))), ("x", S.App(S.Abbr("ab"), S.Abbr("ab")))], 
            S.App(S.Abbr("x"), S.Abbr("ab"))
          )
        ), 
        S.App( S.App(S.Lam("a", S.Var("b")), S.Lam("a", S.Var("b"))),S.Lam("a", S.Var("b")) ),
        "unroll 2")

      val _ = Check.exn (fn () => U.unroll(S.Prog ([("x", S.App(S.Abbr("ab"), S.Abbr("ab"))),("ab", S.Lam("a", S.Var("b")))], S.Tru)), "badTypeRelational")
    in
      TextIO.print "unroll tests done\n"
    end     

  fun desugar () = 
  let
    val _ = Check.expect(D.desugar(S.Var("a")), ULC.Var("a"), "desugar 1")
    val _ = Check.expect(D.desugar(S.Lam("var", S.Tru)), ULC.Lam("var", ULC.Lam("t", ULC.Lam("f", ULC.Var("t")))), "desugar 2")
    val _ = Check.expect(D.desugar(S.Nat(1)), ULC.Lam("s", ULC.Lam("z", ULC.App(ULC.Var("s"), ULC.Lam("s", ULC.Lam("z", ULC.Var("z")))))), "desugar 2")
  in
    TextIO.print "desugar tests done\n"
  end

  fun subst() = ()
  fun full_beta() = ()

  fun all () = 
    let
      val _ = unroll()
      val _ = desugar()
      val _ = subst()
      val _ = full_beta()
      (* cbv *)
      val _ = Check.expect (Compile.cbv "([x x] [y y])",
			    lam ("y", v "y"),
			    "test0")
      val _ = Check.expect (Compile.cbv "(&x &y)",
			    lam ("y", v "y"),
			    "test1")
      val _ = Check.expect (Compile.cbv ":idx=&x; (:idx &y)",
			    lam ("y", v "y"),
			    "test2")
      val _ = Check.expect (Compile.cbv "@t",
			    lam ("t", lam ("f", v "t")),
			    "test3")   
        (* val _ = Check.expect (Compile.cbv ("", ),
			    lam ("t", lam ("f", v "t")),
			    "test3")        *)

      (* full beta *)
      val _ = Check.expect (Compile.fullBeta "([x x] [y y])",
			    lam ("y", v "y"),
			    "fbtest0")
      val _ = Check.expect (Compile.fullBeta "(&x &y)",
			    lam ("y", v "y"),
			    "fbtest1")
      val _ = Check.expect (Compile.fullBeta ":idx=&x; (:idx &y)",
			    lam ("y", v "y"),
			    "fbtest2")
      (* lazy *)

    in
      println "== tests complete"
    end

end
