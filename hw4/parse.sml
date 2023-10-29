structure Parse : sig

  val parse : Token.token list -> Sugary.term

end = struct

  structure T = Token
  structure S = Sugary

  fun early x    = raise Fail (x ^ " ended early")
  fun unclosed x = raise Fail ("unclosed " ^ x)
  fun expected x = raise Fail ("expected " ^ x)
			 
  fun binop T.Plus = S.Add
    | binop T.Minus = S.Subtract
    | binop T.Star = S.Mul
    | binop T.Caret = S.Pow
    | binop T.LessThan = S.Less
    | binop T.GreaterThan = S.Greater
    | binop T.LessEq = S.LessEq
    | binop T.GreaterEq = S.GreaterEq
    | binop T.DoubleAmpersand = S.And
    | binop T.DoublePipe = S.Or
    | binop T.DoubleCaret = S.Xor
    | binop T.DoubleEq = S.Eq
    | binop tok = raise Fail ("expected binary operator, found " ^ T.tos tok)
		      
  fun next [] = NONE
    | next (T.Nat(n) :: tl) = SOME (S.Nat n, tl)
    | next (T.T :: tl) = SOME (S.True, tl)
    | next (T.F :: tl) = SOME (S.False, tl)
    | next (T.LParen :: T.RParen :: tl) = SOME (S.Unit, tl)
    | next (T.ExclamationPoint :: tl) =
       (case next tl
          of SOME (t1, tl') => SOME (S.Not t1, tl')
	   | NONE => early "not")
    | next (T.LBrack :: tl) =
       (case next tl
          of SOME (t1, T.QuestionMark::tl') =>
	       (case next tl'	
	          of SOME (t2, T.Colon::tl'') =>
		      (case next tl''
		         of SOME (t3, T.RBrack::tl''') => SOME (S.Cond (t1, t2, t3), tl''')
			  | SOME _ => unclosed "conditional"
			  | NONE => early "cond (colon)")
	           | SOME _ => expected "colon"
		   | NONE => early "cond (question mark)")
           | SOME (t1, oper::tl') =>
	      (case next tl'
	         of SOME (t2, T.RBrack::tl'') => SOME ((binop oper) (t1, t2), tl'')
		  | SOME _ => unclosed "binary operation" 
		  | NONE => early ("binary operation " ^ T.tos oper))
           | _ => early "bracketed expression")
    | next (T.LParen :: tl) =
       (case next tl
          of SOME (t1, T.Comma::tl') =>
	       (case next tl'
	          of SOME (t2, T.RParen::tl'') => SOME (S.Pair (t1, t2), tl'')
		   | SOME _ => unclosed "pair" 
		   | NONE => early "pair")
	   | SOME _ => expected "comma"
	   | NONE => early "pair")
    | next (T.OneHash :: tl) =
       (case next tl
          of SOME (t1, tl') => SOME (S.First t1, tl')
	   | NONE => early "first")
    | next (T.TwoHash :: tl) =
       (case next tl
          of SOME (t1, tl') => SOME (S.Second t1, tl')
	   | NONE => early "second")
    | next (T.LCurly :: T.Var(x) :: tl) =
        (case next tl
	   of SOME (t1, tl') =>
	      (case next tl'
	        of SOME (t2, T.RCurly::tl'') => SOME (S.Let (x, t1, t2), tl'')
		 | SOME _ => unclosed "let expression"
		 | NONE => early "let expression")
	    | NONE => early "let")
    | next (T.Var(x) :: tl) = SOME (S.Var x, tl)
    | next (tok::_) = raise Fail ("unparseable expression at " ^ T.tos tok)
			   
  fun parse toks =
    let
      fun lp toks =
	(case next toks
	   of SOME (t, []) => t
	    | SOME _ => raise Fail "extra tokens after first term in program"
	    | NONE => raise Fail "empty program")
    in
      lp toks
    end
		     
end



