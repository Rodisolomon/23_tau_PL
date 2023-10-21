structure Parse = struct

  structure T = Token
  structure S = Sweetl

  fun cons1 x (xs, ys) = (x::xs, ys)
		  
  fun next [] = NONE
    | next (T.Var x :: ts) = SOME (S.Var x, ts)
    | next (T.LParen :: ts) =
        (fn (t1, t2, ts') => SOME (S.App (t1, t2), ts')) (next2 T.RParen ts)
    | next (T.LBrack :: ts) =
        (case next2 T.RBrack ts
	  of (S.Var x, t2, ts') => SOME (S.Lam (x, t2), ts')
	   | _ => raise Fail "parse error: lam")
    | next (T.Nat n :: ts) = SOME (S.Nat n, ts)
    | next (T.AtT :: ts) = SOME (S.Tru, ts)
    | next (T.AtF :: ts) = SOME (S.Fls, ts)
    | next (T.ID a :: ts) = SOME (S.ID a, ts)
    | next (T.Abbr a :: ts) = SOME (S.Abbr a, ts)
    | next (tok::_) = raise Fail ("parse error at " ^ T.tos tok)

  and next2 endDelim ts =
    (case next ts
       of SOME (t1, ts1) =>
	    (case next ts1
	       of SOME (t2, t::ts2) => if t=endDelim
				       then (t1, t2, ts2)
				       else raise Fail "next2.0"
		| _ => raise Fail "next2.1")
	| NONE => raise Fail "next2.2")
		     
  fun prog (T.Abbr a :: T.Eq :: ts) =
       (case next ts
	  of SOME (t, T.Semicolon :: ts') => cons1 (a,t) (prog ts')
	   | _ => raise Fail "parse error (prog 1)")
    | prog ts = (case next ts
		   of SOME (t, []) => ([], t)
		    | _ => raise Fail "parse error (prog 2)")
				      
  fun parse ts = S.Prog (prog ts)
		  
end
