structure FullBeta : sig

  val step : ULC.term -> ULC.term option

end = struct

  structure U = ULC

  val sub = Subst.subst
          
  fun step (U.Var _) = NONE
    | step (U.Lam (x, t1)) =
       (case step t1
          of SOME t1' => SOME (U.Lam (x, t1')) 
           | NONE => NONE)
    | step (U.App (U.Lam (x, t1), t2)) = SOME (sub (x, t2, t1))
    | step (U.App (t1, t2)) =
       (case step t1
          of SOME t1' => SOME (U.App (t1', t2))
           | NONE => (case step t2
                        of SOME t2' => SOME (U.App (t1, t2'))
                         | NONE => NONE))

end