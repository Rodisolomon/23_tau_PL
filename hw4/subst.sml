structure Subst : sig

  val fv : ULC.term -> VarSet.set
  val subst : string * ULC.term * ULC.term -> ULC.term

end = struct

  structure U = ULC
  structure S = VarSet
          
  fun fv (U.Var x) = S.ins (x, S.empty) 
    | fv (U.App (t1, t2)) = S.union (fv t1, fv t2)
    | fv (U.Lam (x, t1)) = S.rem (x, fv t1)
          
  fun subst (x, t2, t1) =
    (case t1
       of U.Var y => if x=y then t2 else t1
        | U.App (tA, tB) => U.App (subst (x, t2, tA), subst (x, t2, tB))
        | U.Lam (y, tB) =>
            if x = y then U.Lam (y, tB)
            else if S.mem (y, fv t2) then subst (x, t2, fresh (y, tB))
            else U.Lam (y, subst (x, t2, tB)))
  and fresh (y, tB) =
    let
      val z = Fresh.var ()
    in
      U.Lam (z, subst (y, U.Var z, tB))
    end

end