structure Lazy : sig

  val step : ULC.term -> ULC.term option

end = struct
  structure U = ULC
  structure Sub = Subst
  fun step (U.App (U.Lam(x, t12), t2)) = SOME (Sub.subst(x, t2, t12)) 
    | step (U.App(t1, t2)) =
      (case step t1 of 
        SOME(t1') => SOME(U.App(t1', t2))
        | _ => NONE)
    | step _ = NONE;

end
