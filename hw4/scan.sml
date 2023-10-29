structure Scan : sig

  val scan : string -> Token.token list
	    
end = struct

  structure T = Token

  fun digit cs =
    let
      fun finish (acc, chars) =
	(case Int.fromString (implode (rev acc))
	  of SOME n => SOME (T.Nat n, chars)
	   | NONE => raise Fail ("BUG digit " ^ (implode (rev acc))))
      fun lp ([], acc) = finish (acc, [])
	| lp (chars as c::cs, acc) =
	    if Char.isDigit c
	    then lp (cs, c::acc)
	    else finish (acc, chars)
    in
      lp (cs, [])
    end

  fun var cs =
    let
      fun finish (acc, chars) =
	(case implode (rev acc)
	   of x => SOME (T.Var x, chars))
      fun lp ([], acc) = finish (acc, [])
	| lp (chars as c::cs, acc) =
	    if Char.isLower c
	    then lp (cs, c::acc)
	    else finish (acc, chars)
    in
      lp (cs, [])
    end

  datatype char_category = Space | Lower | Digit | Other
  fun categorize c =
    if Char.isSpace c then Space
    else if Char.isLower c then Lower
    else if Char.isDigit c then Digit
    else Other
						     
      
  fun next [] = NONE
    | next (#"T" :: tl) = SOME (T.T, tl)
    | next (#"F" :: tl) = SOME (T.F, tl)
    | next (#"[" :: tl) = SOME (T.LBrack, tl)
    | next (#"]" :: tl) = SOME (T.RBrack, tl)
    | next (#"(" :: tl) = SOME (T.LParen, tl)
    | next (#")" :: tl) = SOME (T.RParen, tl)
    | next (#"{" :: tl) = SOME (T.LCurly, tl)
    | next (#"}" :: tl) = SOME (T.RCurly, tl)
    | next (#"+" :: tl) = SOME (T.Plus, tl)
    | next (#"-" :: tl) = SOME (T.Minus, tl)
    | next (#"*" :: tl) = SOME (T.Star, tl)
    | next (#"<" :: #"=" :: tl) = SOME (T.LessEq, tl)
    | next (#"<" :: tl) = SOME (T.LessThan, tl)
    | next (#">" :: #"=" :: tl) = SOME (T.GreaterEq, tl)
    | next (#">" :: tl) = SOME (T.GreaterThan, tl)
    | next (#"!" :: tl) = SOME (T.ExclamationPoint, tl)
    | next (#"&" :: #"&" :: tl) = SOME (T.DoubleAmpersand, tl)
    | next (#"|" :: #"|" :: tl) = SOME (T.DoublePipe, tl)
    | next (#"^" :: #"^" :: tl) = SOME (T.DoubleCaret, tl)
    | next (#"^" :: tl) = SOME (T.Caret, tl)
    | next (#"?" :: tl) = SOME (T.QuestionMark, tl)
    | next (#":" :: tl) = SOME (T.Colon, tl)
    | next (#"=" :: #"=" :: tl) = SOME (T.DoubleEq, tl)
    | next (#"," :: tl) = SOME (T.Comma, tl)
    | next (#"1" :: #"#" :: tl) = SOME (T.OneHash, tl)
    | next (#"2" :: #"#" :: tl) = SOME (T.TwoHash, tl)			       
    | next (chars as c::cs) =
       (case categorize c
	  of Space => next cs
	   | Lower => var chars
	   | Digit => digit chars
	   | Other => raise Fail ("scan error: " ^ implode chars))

  fun scan code =
    let
      fun lp cs =
	(case next cs
	   of SOME (tok, cs') => tok :: lp cs'
	    | NONE => [])
    in
      lp (explode code)
    end
      
end
