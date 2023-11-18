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

  fun parseType (concreteType : string) : Type.typ = (case Parse.parse (Scan.scan ("[lam x " ^ concreteType ^ " ()]")) of
                                                        L.Lam (_, tau, _) => tau
                                                      | _ => raise Fail "bug in parseType; this should never happen")
  fun sty () =
    let
      fun chkT t1 t2 = Check.assertT (TypeCheck.subty (parseType t1, parseType t2), t1^"<:"^t2^" (true)")
      fun chkF t1 t2 = Check.assertF (TypeCheck.subty (parseType t1, parseType t2), t1^"<:"^t2^" (false)")
      val _ = chkT "(~a I ~b B)" "(~a I)" (* width subtyping *)
      val _ = chkT "(~a (~x I ~y I) ~b B)" "(~a (~x I) ~b B)" (* depth subtyping ex from class*)
      val _ = chkT "(~x (~a I ~b I) ~y (~m I))" "(~x (~a I))" (* depth subtyping ex from book *)
      val _ = chkT "(~a I ~b B)" "(~b B ~a I)" (* permutation subtyping *)
      val _ = chkT "(~a (~x I ~y I) ~b B ~c I)" "(~b B ~a (~x I))" (* all subtyping ex from ed *)
      val _ = chkF "(~a I)" "(~a I ~b B)"
    in
      println "subtype tests done"
    end

  fun rch_ x = L.tos(RecordCheck.check(Parse.parse(Scan.scan(x))))
  fun rch () =
    let
      val _ = Check.expect (rch_ "[~a 1 ~b T ~c ()]", "(~a 1 ~b T ~c ())", "rch0")
      val _ = Check.expect (rch_ "[~a [~x 1 ~y 2] ~b [~x 3 ~y 4]]", "(~a (~x 1 ~y 2) ~b (~x 3 ~y 4))", "rch1")
      val _ = Check.expect (rch_ "[~a [~a 1 ~b 2] ~b [~a 3 ~b 4]]", "(~a (~a 1 ~b 2) ~b (~a 3 ~b 4))", "rch2")
      val _ = Check.expect (rch_ "[(~a [~a 1 ~b T ~c ()]) + (~b (~b [~a [~a 1 ~b 2] ~b [~a 3 ~b 4]]))]", "[(~a (~a 1 ~b T ~c ()))+(~b (~b (~a (~a 1 ~b 2) ~b (~a 3 ~b 4))))]", "rch3")

      val _ = Check.exn (fn () => rch_ "[~a 1 ~a T ~c ()]", "badrch0")
      val _ = Check.exn (fn () => rch_ "[~a [~x 1 ~y 2] ~b [~x 3 ~x 4]]", "badrch1")
      val _ = Check.exn (fn () => rch_ "[~a [~a 1 ~b 2] ~a [~a 3 ~b 4]]", "badrch2")
      val _ = Check.exn (fn () => rch_ "[~a [~b 1 ~b 2] ~b [~a 3 ~b 4]]", "badrch3")
    in
      println "record check tests done"
    end

  fun typ_ x = T.tos(TypeCheck.typeof(RecordCheck.check(Parse.parse(Scan.scan(x)))))
  fun typ () =
    let
      val _ = Check.expect (typ_ "0", "I", "typ0")
      val _ = Check.expect (typ_ "3", "I", "typ1")
      val _ = Check.expect (typ_ "T", "B", "typ2")
      val _ = Check.expect (typ_ "F", "B", "typ3")
      val _ = Check.expect (typ_ "()", "U", "typ4")
      val _ = Check.expect (typ_ "[2 + 3]", "I", "typ5") (* 5 *)
      val _ = Check.expect (typ_ "[3 - 2]", "I", "typ6") (* 1 *)
      val _ = Check.expect (typ_ "[2 * 3]", "I", "typ7") (* 6 *)
      val _ = Check.expect (typ_ "[20 - [2 + [2 * [2 * 4]]]]", "I", "typ8") (* 2 *)
      val _ = Check.expect (typ_ "[[8 + 4] < [20 - 2]]", "B", "typ19") (* T *)
      val _ = Check.expect (typ_ "!T", "B", "typ10") (* F *)
      val _ = Check.expect (typ_ "!F", "B", "typ11") (* T *)
      val _ = Check.expect (typ_ "[2 == 3]", "B", "typ12") (* F *)
      val _ = Check.expect (typ_ "[2 == 2]", "B", "typ13") (* T *)
      val _ = Check.expect (typ_ "[T ? T : F]", "B", "typ14") (* T *)
      val _ = Check.expect (typ_ "[F ? F : T]", "B", "typ15") (* T *)
      val _ = Check.expect (typ_ "[T ? 0 : 1]", "I", "typ16") (* 0 *)
      val _ = Check.expect (typ_ "[F ? 1 : 0]", "I", "typ17") (* 0 *)
      val _ = Check.expect (typ_ "{x 1 T}", "B", "typ18") (* T *)
      val _ = Check.expect (typ_ "{x 1 x}", "I", "typ19") (* 1 *)
      val _ = Check.expect (typ_ "{x 2 [x * x]}", "I", "typ20") (* 4 *)
      val _ = Check.expect (typ_ "{x 0 {x T !x}}", "B", "typ21") (* F *)
      val _ = Check.expect (typ_ "[~a 1 ~b T ~c ()]", "(~a I ~b B ~c U)", "typ22")
      val _ = Check.expect (typ_ "[~a [~x 1 ~y 2] ~b [~x 3 ~y 4]]", "(~a (~x I ~y I) ~b (~x I ~y I))", "typ23")
      val _ = Check.expect (typ_ "[~a [~a 1 ~b 2] ~b [~a 3 ~b 4]]", "(~a (~a I ~b I) ~b (~a I ~b I))", "typ24")
      val _ = Check.expect (typ_ "(~a [~a 1 ~b T ~c ()])", "I", "typ25") (* 1 *)
      val _ = Check.expect (typ_ "(~b [~a [~a 1 ~b 2] ~b [~a 3 ~b 4]])", "(~a I ~b I)", "typ26") (* (~a 3 ~b 4) *)
      val _ = Check.expect (typ_ "(~b (~b [~a [~a 1 ~b 2] ~b [~a 3 ~b 4]]))", "I", "typ27") (* 4 *)
      val _ = Check.expect (typ_ "[(~a [~a 1 ~b T ~c ()]) + (~b (~b [~a [~a 1 ~b 2] ~b [~a 3 ~b 4]]))]", "I", "typ28") (* 5 *)
      val _ = Check.expect (typ_ "[T ? [~a 1 ~b 2] : [~b 3]]", "(~b I)", "typ29") (* (~a 1 ~b 2)? *)
      val _ = Check.expect (typ_ "([lam x (~c I) (~c x)] [~a () ~b F ~c 3])", "I", "typ30") (* 3 *)
      val _ = Check.expect (typ_ "[lam x (~a I ~b B) [~a (~b x) ~b (~a x)]]", "((~a I ~b B) -> (~a B ~b I))", "typ31") (* ? *)
      val _ = Check.expect (typ_ "[lam x (~a I ~b B ~c U) [~a (~b x)]]", "((~a I ~b B ~c U) -> (~a B))", "typ32") (* ? *)
      val _ = Check.expect (typ_ "([lam y ((~a I ~b B ~c U) -> (~a B)) ()] [lam x (~a I ~b B ~c U) [~a (~b x)]])", "U", "typ33") (* () *)
      val _ = Check.expect (typ_ "([lam y ((~a I ~b B ~c U) -> (~a B)) ()] [lam x (~a I ~b B) [~a (~b x) ~b (~a x)]])", "U", "typ34") (* () *)
      val _ = Check.expect (typ_ "([lam y ((~a I ~b B ~c U) -> (~a B)) (y [~a 2 ~b T ~c ()])] [lam x (~a I ~b B ~c U) [~a (~b x)]])", "(~a B)", "typ35") (* (~a T) *)
      val _ = Check.expect (typ_ "([lam y ((~a I ~b B ~c U) -> (~a B)) (y [~a 2 ~b T ~c ()])] [lam x (~a I ~b B) [~a (~b x) ~b (~a x)]])", "(~a B)", "typ36") (* (~a T ~b 2) *)

      val _ = Check.exn (fn () => typ_ "[(~a [~a T ~b 1 ~c ()]) + (~b (~b [~a [~a 1 ~b 2] ~b [~a 3 ~b 4]]))]", "badTyp0")
    in
      println "type check tests done"
    end

  fun evl_ x = L.tos(Eval.eval(RecordCheck.check(Parse.parse(Scan.scan(x)))))
  fun evl () =
    let
      val _ = Check.expect (evl_ "0", "0", "evl0")
      val _ = Check.expect (evl_ "3", "3", "evl1")
      val _ = Check.expect (evl_ "T", "T", "evl2")
      val _ = Check.expect (evl_ "F", "F", "evl3")
      val _ = Check.expect (evl_ "()", "()", "evl4")
      val _ = Check.expect (evl_ "[2 + 3]", "5", "evl5") (* 5 *)
      val _ = Check.expect (evl_ "[3 - 2]", "1", "evl6") (* 1 *)
      val _ = Check.expect (evl_ "[2 * 3]", "6", "evl7") (* 6 *)
      val _ = Check.expect (evl_ "[20 - [2 + [2 * [2 * 4]]]]", "2", "evl8") (* 2 *)
      val _ = Check.expect (evl_ "[[8 + 4] < [20 - 2]]", "T", "evl19") (* T *)
      val _ = Check.expect (evl_ "!T", "F", "evl10") (* F *)
      val _ = Check.expect (evl_ "!F", "T", "evl11") (* T *)
      val _ = Check.expect (evl_ "[2 == 3]", "F", "evl12") (* F *)
      val _ = Check.expect (evl_ "[2 == 2]", "T", "evl13") (* T *)
      val _ = Check.expect (evl_ "[T ? T : F]", "T", "evl14") (* T *)
      val _ = Check.expect (evl_ "[F ? F : T]", "T", "evl15") (* T *)
      val _ = Check.expect (evl_ "[T ? 0 : 1]", "0", "evl16") (* 0 *)
      val _ = Check.expect (evl_ "[F ? 1 : 0]", "0", "evl17") (* 0 *)
      val _ = Check.expect (evl_ "{x 1 T}", "T", "evl18") (* T *)
      val _ = Check.expect (evl_ "{x 1 x}", "1", "evl19") (* 1 *)
      val _ = Check.expect (evl_ "{x 2 [x * x]}", "4", "evl20") (* 4 *)
      val _ = Check.expect (evl_ "{x 0 {x T !x}}", "F", "evl21") (* F *)
      val _ = Check.expect (evl_ "[~a 1 ~b T ~c ()]", "(~a 1 ~b T ~c ())", "evl22")
      val _ = Check.expect (evl_ "[~a [~x 1 ~y 2] ~b [~x 3 ~y 4]]", "(~a (~x 1 ~y 2) ~b (~x 3 ~y 4))", "evl23")
      val _ = Check.expect (evl_ "[~a [~a 1 ~b 2] ~b [~a 3 ~b 4]]", "(~a (~a 1 ~b 2) ~b (~a 3 ~b 4))", "evl24")
      val _ = Check.expect (evl_ "(~a [~a 1 ~b T ~c ()])", "1", "evl25") (* 1 *)
      val _ = Check.expect (evl_ "(~b [~a [~a 1 ~b 2] ~b [~a 3 ~b 4]])", "(~a 3 ~b 4)", "evl26") (* (~a 3 ~b 4) *)
      val _ = Check.expect (evl_ "(~b (~b [~a [~a 1 ~b 2] ~b [~a 3 ~b 4]]))", "4", "evl27") (* 4 *)
      val _ = Check.expect (evl_ "[(~a [~a 1 ~b T ~c ()]) + (~b (~b [~a [~a 1 ~b 2] ~b [~a 3 ~b 4]]))]", "5", "evl28") (* 5 *)
      val _ = Check.expect (evl_ "[T ? [~a 1 ~b 2] : [~b 3]]", "(~a 1 ~b 2)", "evl29") (* (~a 1 ~b 2)? *)
      val _ = Check.expect (evl_ "([lam x I [x + x]] 2)", "4", "evl30") (* 4 *)
      val _ = Check.expect (evl_ "([lam x (~c I) (~c x)] [~a () ~b F ~c 3])", "3", "evl31") (* 3 *)
      val _ = Check.expect (evl_ "([lam y ((~a I ~b B ~c U) -> (~a B)) ()] [lam x (~a I ~b B ~c U) [~a (~b x)]])", "()", "evl34")
      val _ = Check.expect (evl_ "([lam y ((~a I ~b B ~c U) -> (~a B)) ()] [lam x (~a I ~b B) [~a (~b x) ~b (~a x)]])", "()", "evl35")
      val _ = Check.expect (evl_ "([lam y ((~a I ~b B ~c U) -> (~a B)) (y [~a 2 ~b T ~c ()])] [lam x (~a I ~b B ~c U) [~a (~b x)]])", "(~a T)", "evl36")
      val _ = Check.expect (evl_ "([lam y ((~a I ~b B ~c U) -> (~a B)) (y [~a 2 ~b T ~c ()])] [lam x (~a I ~b B) [~a (~b x) ~b (~a x)]])", "(~a T ~b 2)", "evl37") 
      val _ = Check.expect (evl_ "([lam n I [[n==0] ? 0 : [n - 1]]] 0)", "0", "evl38")
      val _ = Check.expect (evl_ "([lam n I [[n==0] ? 0 : [n - 1]]] 3)", "2", "evl39")
      val _ = Check.expect (evl_ "((fix [lam sum (I -> I) [lam n I [[n==0] ? 0 : (sum [n - 1])]]]) 0)", "0", "evl40")
      val _ = Check.expect (evl_ "((fix [lam sum (I -> I) [lam n I [[n==0] ? 0 : (sum [n - 1])]]]) 4)", "0", "evl41")
      val _ = Check.expect (evl_ "((fix [lam sum (I -> I) [lam n I [[n==0] ? 0 : [n + (sum [n - 1])]]]]) 0)", "0", "evl42")
      val _ = Check.expect (evl_ "((fix [lam sum (I -> I) [lam n I [[n==0] ? 0 : [n + (sum [n - 1])]]]]) 5)", "15", "evl43")
      val _ = Check.expect (evl_ "((fix [lam fact (I -> I) [lam n I [[n==0] ? 1 : [n * (fact [n - 1])]]]]) 0)", "1", "evl44")
      val _ = Check.expect (evl_ "((fix [lam fact (I -> I) [lam n I [[n==0] ? 1 : [n * (fact [n - 1])]]]]) 5)", "120", "evl45")
    in
      println "eval tests done"
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

      val _ = rch ()
      val _ = typ ()
      val _ = sty ()
      val _ = evl ()
    in
      TextIO.print "all tests done\n"
    end
      
end
 