structure Test = struct

  structure T = Token
  structure A = AST

  fun scan () =
    let
      val _ = Check.expect (Scan.scan "Z", [T.Z], "scan0")
      val _ = Check.expect (Scan.scan "Z[", [T.Z, T.LBrack], "scan0")
      val _ = Check.expect (Scan.scan "Z [", [T.Z, T.LBrack], "scan1")
      val _ = Check.expect (Scan.scan "||", [T.DoublePipe], "scan2")
      val _ = Check.expect (Scan.scan " | |", [T.DoublePipe], "scan3")
      val _ = Check.exn (fn () => Scan.scan "~", "badScan00")
      (* write more scan tests here *)
    in
      TextIO.print "scan tests done\n"
    end

  fun parse () =
    let
      val _ = Check.expect (Parse.parse [T.Z], A.Zero, "parse0")
      val _ = Check.exn (fn () => Parse.parse [T.LBrack], "badParse0")
      (* write more parse tests here *)
    in
      TextIO.print "parse tests done\n"
    end

  fun eval () =
    let
      val _ = Check.expect (Eval.eval A.Zero, [A.Zero], "eval0")
      (* write more eval tests here *)
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
