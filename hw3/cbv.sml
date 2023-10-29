structure CBV : sig

  val step : ULC.term -> ULC.term option

end = struct
  structure U = ULC
  structure Sub = Subst
  (* U.Lam(x2, t22) is v2 in this case *)
  fun step (U.App(U.Lam(x1, t12), U.Lam(x2, t22)))  =  SOME (Sub.subst(x1, U.Lam(x2, t22), t12))
    | step (U.App(t1, t2)) =
      (case step t1 of 
          SOME (t1') => SOME (U.App(t1', t2))
        | NONE => (case step t2 of
            SOME (t2') => SOME (U.App(t1, t2'))
          | NONE => NONE)
      )
    | step _ = NONE;


end
