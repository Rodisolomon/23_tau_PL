structure Scan = struct

  structure T = Token

  fun skipComment [] = []
    | skipComment (#"\n" :: cs) = cs
    | skipComment cs = cs

  fun collect test [] = ([], [])
    | collect test (chars as c::cs) =
        if test c
	then (fn (cs1,cs2) => (c::cs1,cs2)) (collect test cs)
	else ([], chars)

  fun var cs =
    (case collect Char.isLower cs
       of ([], _) => raise Fail "bug: var"
	| (cs1, cs2) => SOME (T.Var (implode cs1), cs2))
	       
  fun nat cs =
    (case collect Char.isDigit cs
       of (ds, cs') =>
	    (case Int.fromString (implode ds)
	       of SOME n => SOME (T.Nat n, cs')
		| NONE => raise Fail "bug: nat"))
			     	       
  fun next [] = NONE
    | next (#"(" :: cs) = SOME (T.LParen, cs)
    | next (#")" :: cs) = SOME (T.RParen, cs)
    | next (#"[" :: cs) = SOME (T.LBrack, cs)
    | next (#"]" :: cs) = SOME (T.RBrack, cs)
    | next (#"@" :: #"t" :: cs) = SOME (T.AtT, cs)
    | next (#"@" :: #"f" :: cs) = SOME (T.AtF, cs)
    | next (#"&" :: cs) =
        (case collect Char.isLower cs
	  of ([], _) => raise Fail "scan error at &"
	   | (caps, cs') => SOME (T.ID (implode caps), cs'))
    | next (#":" :: cs) =
        (case collect Char.isLower cs
	  of ([], _) => raise Fail "scan error at :"
	   | (lowers, cs') => SOME (T.Abbr (implode lowers), cs'))
    | next (#"=" :: cs) = SOME (T.Eq, cs)
    | next (#";" :: cs) = SOME (T.Semicolon, cs)
    | next (#"/" :: #"/" :: cs) = next (skipComment cs)
    | next (chars as c::cs) =
        if Char.isSpace c then next cs
	else if Char.isDigit c then nat chars
	else if Char.isLower c then var chars
	else raise Fail ("scan error at " ^ implode [c])
		   
  fun scan code =
    let
      fun lp cs = (case next cs
		     of SOME (tok, cs') => tok :: lp cs'
		      | NONE => [])
    in
      lp (explode code)
    end
end
