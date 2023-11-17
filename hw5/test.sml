structure Test = struct

  (* tests here... *)
  structure L = L23RR
  structure T = Type
  structure RC = RecordCheck
  structure TC = TypeCheck

  fun println s = (print s; print "\n")

  fun testing x = println ("testing " ^ x ^ "...")

  fun parseType (concreteType : string) : Type.typ =
   (case Parse.parse (Scan.scan ("[lam x " ^ concreteType ^ " ()]"))
      of L.Lam (_, tau, _) => tau
       | _ => raise Fail "bug in parseType; this should never happen")

  fun subty () =
    let
      fun chkT t1 t2 = Check.assertT (TypeCheck.subty (parseType t1, parseType t2), t1^"<:"^t2^" (true)")
      fun chkF t1 t2 = Check.assertF (TypeCheck.subty (parseType t1, parseType t2), t1^"<:"^t2^" (false)")
      val _ = testing "subty"
      val _ = chkT "(~a I ~b B)" "(~a I)"
      val _ = chkF "(~a I)" "(~a I ~b B)"
    in
      println "subty tests done"
    end

  fun recordCheck () = 
    let
      val _ = Check.expect(RC.check(L.Record([("a", L.True), ("b", L.False)])), RC.check(L.Record([("a", L.True), ("b", L.False)])), "simple true")
      val _ = Check.expect(RC.check(L.Record([("a", L.Record([("a", L.True), ("b", L.False)])), ("b", L.False)])), RC.check(L.Record([("a", L.Record([("a", L.True), ("b", L.False)])), ("b", L.False)])), "nested true")

      val _ = Check.exn (fn () => RC.check(L.Record([])), "empty list")
      val _ = Check.exn (fn () => RC.check(L.Record([("a", L.True), ("a", L.False)])), "repetition record")
      val _ = Check.exn (fn () => RC.check(L.Record([("a", L.Record([("a", L.True), ("a", L.False)])), ("b", L.False)])), "nested repetition record")
    in
      TextIO.print "record check test done\n"
    end

  fun typeCheck () =
    let
      val _ = Check.expect(TC.subty(T.Record([("a", T.Bool), ("b", T.Bool)]), T.Record([("a", T.Bool)])), true, "subty1")
      val _ = Check.expect(TC.subty(T.Record([("a", T.Record([("a", T.Bool), ("b", T.Bool)])), ("b", T.Bool)]), T.Record([("a", T.Record([("a", T.Bool)]))])), true, "subty2")
      val _ = Check.expect(TC.subty(T.Record([("a", T.Record([("a", T.Bool), ("b", T.Bool)])), ("b", T.Bool)]), T.Record([("a", T.Record([("a", T.Bool), ("b", T.Bool)]))])), true, "subty3")
      val _ = Check.expect(TC.subty(T.Record([("a", T.Bool)]), T.Record([("a", T.Bool), ("b", T.Bool)])), false, "subtyFalse1")

      val _ = Check.expect(TC.typeof(L.Int(1)), T.Int, "typecheckSimple")
      val _ = Check.expect(TC.typeof(L.Add(L.Int(1), L.Int(0))), T.Int, "typecheckArith")
      val _ = Check.expect(TC.typeof(L.Lam("x", T.Int, L.Add(L.Var("x"), L.Int(0)))), T.Function(T.Int, T.Int), "typecheckLam")
      val _ = Check.expect(TC.typeof(L.App(L.Lam("x", T.Int, L.Add(L.Var("x"), L.Int(0))), L.Int(0))), T.Int, "typecheckApp1")
      val rec1 = L.Record([("a", L.True), ("b", L.False)])
      val rec2 = L.Record([("a", L.True), ("b", L.False), ("c", L.Int(0))])
      val tau1 = T.Record([("a", T.Bool), ("b", T.Bool)])
      val testLam = L.Lam("x", tau1, L.Int(0))
      val _ = Check.expect(TC.typeof(L.App(testLam, rec2)), T.Int, "typecheckApp2")
      val _ = Check.expect(TC.typeof(L.Fix(L.Lam("x", T.Int, L.Add(L.Var("x"), L.Int(0))))), T.Int, "typecheckFix")
      val _ = Check.expect(TC.typeof(rec1), tau1, "typecheckRecord")
      val _ = Check.expect(TC.typeof(L.Select("a", rec1)), T.Bool, "typecheckSelect")

    in
      TextIO.print "type check test done\n"

    end
  fun all () =
    let
      val _ = subty ()
      val _ = recordCheck()
      val _ = typeCheck ()
      val _ = Check.expect(Compile.code("[[1 + 2] - [1 + 0]]"), Compile.code("2"), "plus+minus")
      val _ = Check.expect(Compile.code "[2 * 2]", Compile.code("4"), "mul 1")

      val fls = Compile.code "F"
      val tru = Compile.code "T"
      val _ = Check.expect(Compile.code "!T", fls, "cond1")
      val _ = Check.expect(Compile.code "[1 < 2]", tru, "less")
      val _ = Check.expect(Compile.code "[2 == 2]", tru, "Eq")
      val _ = Check.expect(Compile.code "[3 == 2]", fls, "Eq2")

      val _ = Check.expect(Compile.code "[T?2:1]", Compile.code("2"), "Cond1")
      val _ = Check.expect(Compile.code "[F?2:1]", Compile.code("1"), "Cond2")

      val _ = Check.expect(Compile.code "{x 1 [2+x]}", Compile.code("3"), "let")

      (* scope, fix, record *)
      val _ = Check.expect(
        Compile.code "{x 2 {x 3 [x+x]}}",
        Compile.code "6",
        "scoped let"
      )
      val _ = Check.expect(
        Compile.code "((fix[ lam f (I -> I) [ lam n I [ [n==0]?1:[n*(f [n-1])] ] ] ]) 3)",
        Compile.code "6",
        "fix factorial of 6"
      )

      val _ = Check.expect(
        Compile.code "(~x [~x 1 ~y T])",
        Compile.code "1",
        "record creation and integer field selection"
      )

      val _ = Check.expect(
        Compile.code "(~y [~x 1 ~y T])",
        Compile.code "T",
        "record creation and boolean field selection"
      )


    in
      TextIO.print "all tests done\n"
    end
      
end
 