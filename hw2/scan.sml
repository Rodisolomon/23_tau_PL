structure Scan : sig

  val next : char list -> (Token.token * char list) option
  val scan : string -> Token.token list
	    
end = struct

  structure T = Token

  fun next [] = NONE
    | next (#"T" :: tl) = SOME (T.T, tl)
    | next (#"F" :: tl) = SOME (T.F, tl)
    | next (#"[" :: tl) = SOME (T.LBrack, tl)
    | next (#"]" :: tl) = SOME (T.RBrack, tl)
    | next (#"(" :: tl) = SOME (T.LParen, tl)
    | next (#")" :: tl) = SOME (T.RParen, tl)
    | next (#"+" :: tl) = SOME (T.Plus, tl)
    | next (#"-" :: tl) = SOME (T.Minus, tl)
    | next (#"<" :: #"=" :: tl) = SOME (T.LessEq, tl)
    | next (#"<" :: tl) = SOME (T.LessThan, tl)
    | next (#">" :: #"=" :: tl) = SOME (T.GreaterEq, tl)
    | next (#">" :: tl) = SOME (T.GreaterThan, tl)
    | next (#"!" :: tl) = SOME (T.ExclamationPoint, tl)
    | next (#"&" :: #"&" :: tl) = SOME (T.DoubleAmpersand, tl)
    | next (#"|" :: #"|" :: tl) = SOME (T.DoublePipe, tl)
    | next (#"^" :: #"^" :: tl) = SOME (T.DoubleCaret, tl)
    | next (#"?" :: tl) = SOME (T.QuestionMark, tl)
    | next (#":" :: tl) = SOME (T.Colon, tl)
    | next (#"=" :: #"=" :: tl) = SOME (T.DoubleEq, tl)
    | next (#"," :: tl) = SOME (T.Comma, tl)
    | next (#"1" :: #"#" :: tl) = SOME (T.OneHash, tl)
    | next (#"2" :: #"#" :: tl) = SOME (T.TwoHash, tl)			       
    | next (c::cs) =
        if Char.isSpace c
	then next cs
	else (if Char.isDigit c
	      then raise Fail "todo"
	      else raise Fail ("scan error: " ^ implode (c::cs)))

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
