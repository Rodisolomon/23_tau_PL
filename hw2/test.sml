structure Test = struct

  structure T = Token
  structure S = Sugary
  structure D = Desugared
  structure E = Eval
		  
  fun scan () =
    let
      val _ = Check.expect (Scan.scan "12", [T.Nat 12], "scan12")
      val _ = Check.exn (fn () => Scan.scan "12#", "badScan12#")
      val _ = Check.exn (fn () => Scan.scan "~", "badScan00")
      (* write more scan tests here *)
    in
      TextIO.print "scan tests done\n"
    end

  fun parse () =
    let
      val _ = Check.expect (Parse.parse [T.Nat 12], S.Nat 12, "parse12")
      val _ = Check.exn (fn () => Parse.parse [T.LBrack], "badParse0")
      (* write more parse tests here *)
    in
      TextIO.print "parse tests done\n"
    end

  fun typ () =
    let
      val _ = Check.expect (TypeCheck.typeof (S.Nat 12), Type.Nat, "type12") 
      val _ = Check.expect (TypeCheck.typeof (S.Add (S.Nat 1, S.Nat 12)), Type.Nat, "GoodTypeRelational") 
      val _ = Check.expect (TypeCheck.typeof (S.Eq (S.Nat 1, S.True)), Type.Bool, "GoodTypeEq") 
      val _ = Check.expect (TypeCheck.typeof (S.Cond (S.True, S.Nat 12, S.False)), Type.Nat, "GoodTypeCond") 

      val _ = Check.exn (fn () => TypeCheck.typeof (S.Add (S.True, S.Nat 12)), "badTypeRelational")
      val _ = Check.exn (fn () => TypeCheck.typeof (S.Eq (S.Add (S.True, S.Nat 12), S.Nat 12)), "badTypeRelational")
      val _ = Check.exn (fn () => TypeCheck.typeof (S.Cond (S.Nat 12, S.True, S.False)), "badTypeConditional")

    in
      TextIO.print "type tests done\n"
    end

  fun desugar () =
    let
      val desugar = Desugar.desugar
      val _ = Check.expect (desugar (S.Nat 0), D.Zero, "desugar0")
      val _ = Check.expect (desugar (S.Nat 3), D.Succ (D.Succ (D.Succ (D.Zero))), "desugarNatNum")
      val _ = Check.expect (desugar (S.Subtract ((S.Nat 0), S.Nat 3)), (D.Subtract (D.Zero, D.Succ (D.Succ (D.Succ (D.Zero))))), "desugarNestedSubtract")
      val _ = Check.expect (desugar (S.Subtract ((S.Nat 0), S.Nat 3)), (D.Subtract (D.Zero, D.Succ (D.Succ (D.Succ (D.Zero))))), "desugarNestedSubtract")
      val _ = Check.expect (desugar (S.GreaterEq ((S.Nat 0), (S.Nat 0))), (D.Cond(D.Cond(D.Cond(D.Less(D.Zero, D.Zero), D.Succ (D.Zero), D.Zero), D.Succ (D.Zero), D.Eq(D.Zero, D.Zero)), D.Succ (D.Zero), D.Zero)), "desugarGreaterEq")
    in
      TextIO.print "desugar tests done\n"
    end
			            
  fun eval () =
    let
      val _ = Check.expect (Eval.result D.Zero, Eval.Value D.Zero, "eval0")
      (* write more eval tests here *)
    in
      TextIO.print "eval tests done\n"
    end

  fun compile () =
    let
      fun value typ program result =
	Check.expect (Compile.code program, (E.Value result, typ), "compile"^program)
      val natval = value Type.Nat
      val boolval = value Type.Bool
      val _ = natval "0" D.Zero 
      (* write more compile tests here *)
    in
      TextIO.print ("compile tests done\n")
    end
      
  fun all () =
    let
      val _ = scan ()
      val _ = parse ()
      val _ = typ ()
      val _ = desugar ()
      val _ = eval ()
      val _ = compile ()
    in
      TextIO.print "all tests done\n"
    end
      
end
