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
 