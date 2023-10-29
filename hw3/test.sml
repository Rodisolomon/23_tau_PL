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
  val a = ULC.App


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
      val _ = Check.expect(D.desugar(S.Nat(1)), ULC.Lam("s", ULC.Lam("z", ULC.App(ULC.Var("s"), ULC.Var("z")))), "desugar 3")
    in
      TextIO.print "desugar tests done\n"
    end


  fun all () = 
    let
      val _ = unroll()
      val _ = desugar()
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
      val _ = Check.expect (Compile.cbv ":aa = &a;:bb = &b;(:aa :bb)",
			    lam ("b", v "b"),
			    "test4")  
      val _ = Check.expect (Compile.cbv ":or = [b [c ((b @t) c)]];((:or @f) @f)",
			    lam ("t", lam ("f", v "f")),
			    "test5") 
      val _ = Check.expect (Compile.cbv "([x x] y)",
			    a (lam ("x", v "x"), v "y"),
			    "test6")
      (* val _ = Check.expect (Compile.fullBeta ":add = [m [n [s [z ((m s) ((n s) z))]]]]; ((:add 1) 1)",
        lam("s", lam("z", a (v ("s"), lam("s", lam("z", v ("z")))))),
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
        val _ = Check.expect (Compile.fullBeta ":aa = &a;:bb = &b;(:aa :bb)",
			    lam ("b", v "b"),
			    "fbtest3")  
      val _ = Check.expect (Compile.fullBeta ":or = [b [c ((b @t) c)]];((:or @f) @f)",
			    lam ("t", lam ("f", v "f")),
			    "fbtest4") 
      val _ = Check.expect (Compile.fullBeta "([x x] y)",
			    v "y",
			    "fbtest5") 

      (* lazy *)
      val _ = Check.expect (Compile.lazy "([x x] [y y])",
			    lam ("y", v "y"),
			    "lztest0")
      val _ = Check.expect (Compile.lazy "(&x &y)",
			    lam ("y", v "y"),
			    "lztest1")
      val _ = Check.expect (Compile.lazy ":idx=&x; (:idx &y)",
			    lam ("y", v "y"),
			    "lztest2")
      val _ = Check.expect (Compile.lazy ":aa = &a;:bb = &b;(:aa :bb)",
          lam ("b", v "b"),
          "lztest3")  
      val _ = Check.expect (Compile.lazy ":or = [b [c ((b @t) c)]];((:or @f) @f)",
			    lam ("t", lam ("f", v "f")),
			    "lztest5") 
      val _ = Check.expect (Compile.lazy "([x x] y)",
			    v "y",
			    "lztest6") 
    in
      println "== start compiled file tests...";
      Compile.fullBetaFile "SweetlCode/add.sw";
      Compile.cbvFile "SweetlCode/add.sw";
      Compile.lazyFile "SweetlCode/add.sw";
      println "== tests complete"
    end

end
