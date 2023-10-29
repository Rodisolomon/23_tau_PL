structure FullBeta : sig

  val step : ULC.term -> ULC.term option
  val isNF : ULC.term -> bool
  val isNANF : ULC.term -> bool
  val isNA : ULC.term -> bool
end = struct

  structure U = ULC
  structure Sub = Subst

  fun isNA (U.App(t1, t2)) = true
    | isNA (U.Var(s)) = true
    | isNA _ = false

  fun isNANF (U.Var(x)) = true
    | isNANF (U.App(t1, t2)) =
      if isNANF t1 then 
        if isNF t2 then true
        else false
      else false
    | isNANF _ = false
    
  and isNF (U.Lam(x, y)) = 
      if isNF y then true
      else false
    | isNF t = 
      if isNANF t then true
      else false

  fun step (U.App(U.Lam(x, t12), t2)) = SOME (Sub.subst(x, t2, t12))
    | step (U.Lam(x, t1)) = 
      (case step t1 of 
          SOME (t1') => SOME (U.Lam(x, t1'))
        | _ => NONE)
    | step (U.App(t1, t2)) = 
      if (isNA (t1)) then
        (case step t1 of
            SOME (t1') => SOME (U.App(t1', t2))
          | NONE =>         
            (case step t2 of
              SOME (t2') => SOME (U.App(t1, t2'))
            | _ => NONE
            )
        )      
      else if (isNANF (t1)) then
        (case step t2 of
            SOME (t2') => SOME (U.App(t1, t2'))
          | _ => NONE
        )
      else
        NONE
    | step _ = NONE;  


end
