structure Test = struct

  structure T = Token
  structure A = AST

  fun scan () =
    let
      val _ = Check.expect (Scan.scan "Z", [T.Z], "scan0")
      val _ = Check.expect (Scan.scan "Z[", [T.Z, T.LBrack], "scan0")
      val _ = Check.expect (Scan.scan "Z [", [T.Z, T.LBrack], "scan1")
      val _ = Check.expect (Scan.scan "||", [T.DoublePipe], "scan2")
      val _ = Check.expect (Scan.scan "[Z+Z]", [T.LBrack, T.Z, T.Plus, T.Z, T.RBrack], "scan4")
      val _ = Check.expect (Scan.scan "[[Z<SZ]||F]", [T.LBrack, T.LBrack, T.Z, T.LessThan, T.S, T.Z, T.RBrack, T.DoublePipe, T.F, T.RBrack], "scan5")

      val _ = Check.exn (fn () => Scan.scan "~", "badScan00")
      val _ = Check.exn (fn () => Scan.scan "| |", "badScan01")
      (* write more scan tests here *)
    in
      TextIO.print "scan tests done\n"
    end

  fun parse () =
    let
      val _ = Check.expect (Parse.parse [T.Z], A.Zero, "parse0")
      val _ = Check.expect (Parse.parse [T.LBrack, T.Z, T.Plus, T.Z, T.RBrack], A.Add (A.Zero, A.Zero), "parse1")
      val _ = Check.expect (Parse.parse [T.LBrack, T.LBrack, T.Z, T.Plus, T.Z, T.RBrack, T.Plus, T.Z, T.RBrack], A.Add (A.Add(A.Zero, A.Zero), A.Zero), "parse2")

      val _ = Check.expect (Parse.parse [T.LBrack, T.Z, T.LessThan, T.Z, T.RBrack], A.Less (A.Zero, A.Zero) , "parse3")
      val _ = Check.expect (Parse.parse [T.LBrack, T.Z, T.LessThan, T.S, T.Z, T.RBrack], A.Less (A.Zero, A.Succ (A.Zero)) , "parse4")

      (* [[Z < SZ] || F] *)
      (* eg. [T.LBrack, T.LBrack, T.Z, T.LessThan, T.S, T.Z, T.RBrack, T.DoublePipe, T.F, T.RBrack] *)
      val _ = Check.expect (Parse.parse [T.LBrack, T.LBrack, T.Z, T.LessThan, T.S, T.Z, T.RBrack, T.DoublePipe, T.F, T.RBrack], A.Or (A.Less (A.Zero, A.Succ (A.Zero)), A.False) , "parse5")

      val _ = Check.expect (Parse.parse [T.LBrack, T.Z, T.QuestionMark, T.Z, T.Colon, T.Z, T.RBrack], A.Cond(A.Zero, A.Zero, A.Zero) , "parse6")

      val _ = Check.exn (fn () => Parse.parse [T.LBrack], "badParse0")
    in
      TextIO.print "parse tests done\n"
    end

  fun eval () =
    let
      (* check isNV and isV *)
      val _ = Check.expect (Eval.isNV A.Zero, true, "eval0")
      val _ = Check.expect (Eval.isNV (A.Succ A.Zero), true, "eval0")
      val _ = Check.expect (Eval.isNV A.True, false, "eval0")
      val _ = Check.expect (Eval.isV A.True, true, "eval0")
      val _ = Check.expect (Eval.isV (A.Succ A.Zero), true, "eval0")

      (* check step *)
      val _ = Check.expect (Eval.step (A.Pred(A.Succ (A.Zero))), SOME A.Zero, "eval1")
      val _ = Check.expect (Eval.step (A.Less(A.Zero, A.Succ (A.Zero))), SOME A.True, "eval1")
      (* check step *)
      val _ = Check.expect (Eval.eval A.Zero, [A.Zero], "eval0")
      val _ = Check.expect (Eval.eval (A.Add(A.Pred(A.Zero), A.Zero)), [A.Add (A.Pred(A.Zero), A.Zero), A.Add (A.Zero, A.Zero), A.Zero], "eval1.2+2.3")


      (* stuck term *)
      val _ = Check.expect (Eval.eval (A.Greater(A.Succ(A.True), A.Pred(A.Zero))), [A.Greater(A.Succ(A.True), A.Pred(A.Zero))], "eval2.2S")
      val _ = Check.expect (Eval.eval (A.Subtract(A.And (A.True, A.False), A.Or (A.False, A.False))), [A.Subtract(A.And (A.True, A.False), A.Or (A.False, A.False)), A.Subtract(A.False, A.Or (A.False, A.False)), A.Subtract(A.False, A.False)], "eval3.3S")


    in
      TextIO.print "eval tests done\n"
    end

  fun compile () =
    let
      val _ = Check.expect (Compile.code "SZ", [A.Succ A.Zero], "compile0")
      (* write more eval tests here *)
    in
      TextIO.print ("compile tests done\n")
    end
      
  fun all () =
    let
      val _ = scan ()
      val _ = parse ()
      val _ = eval ()
      val _ = compile ()
    in
      TextIO.print "all tests done\n"
    end
      
end
