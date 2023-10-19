structure Test = struct

  structure T = Token
  structure S = Sugary
  structure D = Desugared
  structure E = Eval
		  
  fun scan () =
    let
      val _ = Check.expect (Scan.scan "12", [T.Nat 12], "scan12")
      val _ = Check.expect (Scan.scan "[1>2]", [T.LBrack, T.Nat 1, T.GreaterThan, T.Nat 2, T.RBrack], "scan12")
      val _ = Check.expect (Scan.scan "[1==2]", [T.LBrack, T.Nat 1, T.DoubleEq, T.Nat 2, T.RBrack], "scanEq")
      val _ = Check.exn (fn () => Scan.scan "12#", "badScan12#")
      val _ = Check.exn (fn () => Scan.scan "~", "badScan00")
      (* write more scan tests here *)
    in
      TextIO.print "scan tests done\n"
    end

  fun parse () =
    let
      val _ = Check.expect (Parse.parse [T.Nat 12], S.Nat 12, "parse12")
      val _ = Check.expect (Parse.parse [T.LBrack, T.Nat 1, T.DoubleEq, T.Nat 2, T.RBrack], S.Eq (S.Nat 1, S.Nat 2), "parse12")
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
      val _ = Check.expect (TypeCheck.typeof (S.Eq (S.Nat 0, S.Nat 1)), Type.Bool, "GoodTypeEq2") 
      val _ = Check.expect (TypeCheck.typeof (S.Cond (S.True, S.Nat 12, S.False)), Type.Nat, "GoodTypeCond") 
      val _ = Check.expect (TypeCheck.typeof (S.First(S.Pair(S.Nat 1, S.True))), Type.Nat, "GoodTypeTuple") 
      val _ = Check.expect (TypeCheck.typeof (S.Greater (S.Nat 1, S.Nat 0)), Type.Bool, "GoodTypeGreater") 

      val _ = Check.exn (fn () => TypeCheck.typeof (S.Add (S.True, S.Nat 12)), "badTypeRelational")
      val _ = Check.exn (fn () => TypeCheck.typeof (S.Eq (S.Add (S.True, S.Nat 12), S.Nat 12)), "badTypeRelational")
      val _ = Check.exn (fn () => TypeCheck.typeof (S.Cond (S.Nat 12, S.True, S.False)), "badTypeConditional")
      val _ = Check.exn (fn () => TypeCheck.typeof (S.First(S.Nat 1)), "badTypeConditional")


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
      val _ = Check.expect (Eval.result (D.Pair (D.Succ (D.Zero), D.Succ (D.Zero))), Eval.Value (D.Pair (D.Succ (D.Zero), D.Succ (D.Zero))), "eval0")
      val _ = Check.expect (Eval.result (D.Pair (D.Succ (D.Zero), D.Succ (D.Zero))), Eval.Value (D.Pair (D.Succ (D.Zero), D.Succ (D.Zero))), "eval0")
      val _ = Check.expect (Eval.result (D.Cond (D.Less (D.Zero, D.Succ (D.Zero)), D.Zero, D.Succ (D.Zero))), Eval.Value (D.Zero), "evalGreaterEq")

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
      val _ = boolval "[T&&T]" (D.Succ D.Zero)
      val _ = boolval "[2==1]" (D.Zero)
      val _ = boolval "[2>1]" (D.Succ D.Zero)
      val _ = boolval "[0==0]" (D.Succ D.Zero)
      val _ = boolval "[9<10]" (D.Succ D.Zero)
      val _ = boolval "[2<1]" (D.Zero)

      val _ = natval "0" D.Zero 
      val _ = Check.expect (Compile.code "1",
                      (E.Value (D.Succ D.Zero), Type.Nat),
                      "compile1")
      val _ = natval "[1+1]" (D.Succ (D.Succ D.Zero))
      val _ = natval "[2-1]" (D.Succ D.Zero)




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
