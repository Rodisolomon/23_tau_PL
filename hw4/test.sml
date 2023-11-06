structure Test = struct

  (* tests here... *)
      
  structure S = Sugary
  structure D = Desugar
  structure U = ULC
  val lam = ULC.Lam
  val v   = ULC.Var
  val a = ULC.App
  fun println s = TextIO.print (s ^ "\n")

  fun desugar () = 
    let
      val _ = Check.expect(D.desugar(S.Var("a")), ULC.Var("a"), "desugar 1")
      val _ = Check.expect(D.desugar(S.True), ULC.Lam("t", ULC.Lam("f", ULC.Var("t"))), "desugar 2")
      val _ = Check.expect(D.desugar(S.Nat(1)), ULC.Lam("s", ULC.Lam("z", ULC.App(ULC.Var("s"), ULC.Var("z")))), "desugar 3")

      (* condition *)
      val _ = println (U.tos(D.desugar(S.And(S.True, S.False))))
      val _ = println (U.tos(D.desugar(S.Not(S.True))))

      (* compare *)
      val _ = println (U.tos(D.desugar(S.Greater(S.Nat(1), S.Nat(2)))))

    in

      TextIO.print "desugar tests done\n"
    end


  fun all () =
    let
      val _ = desugar()
      (* calculation *)
      val _ = Check.expect(Compile.code("[[1 + 2] - [1 + 0]]"), Compile.code("2"), "plus+minus")
      val _ = Check.expect(Compile.code "[2 + 2]", Compile.code("4"), "plus 1")
      val _ = Check.expect(Compile.code "[2 * 2]", Compile.code("4"), "mul 1")
      val _ = Check.expect(Compile.code "[2 ^ 2]", Compile.code("4"), "power 1")
      val _ = Check.expect(Compile.code "[2 * 0]", Compile.code("0"), "mul 2")
      val _ = Check.expect(Compile.code "[2 ^ 0]", Compile.code("1"), "power 2")
      val _ = Check.expect(Compile.code "[2 ^ 3]", Compile.code("8"), "power 2")


      (* condition *)
      val fls = Compile.code "F"
      val tru = Compile.code "T"
      val _ = Check.expect(Compile.code "!T", fls, "cond1")
      val _ = Check.expect(Compile.code "[T && F]", fls, "cond2")
      val _ = Check.expect(Compile.code "[T && F]", fls, "cond3")
      val _ = Check.expect(Compile.code "[T && [F || T]]", tru, "cond4")
      val _ = Check.expect(Compile.code "[T ^^ F]", tru, "cond4")

      (* compare *)
      val _ = Check.expect(Compile.code "[1 >= 2]", fls, "greaterEq")
      val _ = Check.expect(Compile.code "[1 > 2]", fls, "greater")
      val _ = Check.expect(Compile.code "[1 < 2]", tru, "less")
      val _ = Check.expect(Compile.code "[2 <= 2]", tru, "lessEq")
      val _ = Check.expect(Compile.code "[2 == 2]", tru, "Eq")
      val _ = Check.expect(Compile.code "[3 == 2]", fls, "Eq2")

      (* others *)
      val _ = Check.expect(Compile.code "[T?2:1]", Compile.code("2"), "Cond1")
      val _ = Check.expect(Compile.code "[F?2:1]", Compile.code("1"), "Cond2")
      val _ = Check.expect(Compile.code "1#(1,2)", Compile.code("1"), "pair")
      val _ = Check.expect(Compile.code "2#(1,[1+0])", Compile.code("1"), "pair")

      val _ = Check.expect(Compile.code "[![ [[3^2]-1] == 2 ] || F]", tru, "combo")

      val tokens  = Scan.scan "{x 1 [2+x]}"
      val sweet   = Parse.parse tokens
      val _ = println (U.tos(D.desugar(sweet)))
      val _ = println (Type.tos(TypeCheck.typeof (TypeEnv.empty, sweet)))
      val _ = Compile.code ("{x 1 [2+x]}")
      val _ = Check.expect(Compile.code "{x 1 [2+x]}", Compile.code("3"), "let")

    in
    
      TextIO.print "all tests done\n"
      
    end 
      
end
