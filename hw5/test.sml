structure Test = struct

  (* tests here... *)
  structure L = L23RR
  structure T = Type
  structure RC = RecordCheck
  structure TC = TypeCheck
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
      val _ = recordCheck()
    in
      TextIO.print "all tests done\n"
    end
      
end
 