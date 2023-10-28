structure Subst : sig

  val fv : ULC.term -> VarSet.set
  val subst : string * ULC.term * ULC.term -> ULC.term
  val freshSpecial: string * ULC.term -> string * ULC.term
end = struct
		  
  fun fv input =
    case input of
        ULC.Var(str) => VarSet.singleTerm(str)
      | ULC.App(x1, x2) => VarSet.union(fv x1, fv x2) (* new set *)
      | ULC.Lam(str, set) => VarSet.rem(str, fv set)

  fun subst (x, s, ULC.Var(y)) = if x=y then s else ULC.Var(y)
    | subst (x, s, ULC.App(t1, t2)) = ULC.App(subst(x, s, t1), subst(x, s, t2))
    | subst (x, s, ULC.Lam(y, t1)) = if x=y then ULC.Lam(x, t1)
      else if VarSet.mem(y, fv s) then subst (x, s, ULC.Lam(freshSpecial (y, t1)))
      else ULC.Lam(y, subst(x, s, t1))
    and freshSpecial (y, t1) =
        let
            val newVar = Fresh.var()
        in
            (newVar, subst(y, ULC.Var newVar, t1))
        end

end


  (* fun freshSpecial (y, new_y, ULC.App(t1, t2)) = ULC.App(freshSpecial(y, new_y, t1), freshSpecial(y, new_y, t2))
    | freshSpecial (y, new_y, ULC.Lam(t1, t2)) = if t1=ULC.Var(y) then ULC.Lam(new_y, freshSpecial(y, new_y, t2)) else ULC.Lam(t1, t2)
    | freshSpecial (y, new_y, ULC.Var(t1)) = if t1=y then ULC.Var(new_y) else ULC.Var(y)

  fun freshSpecialWrapper(y, t1) = 
    let 
      val new_y : ULC.term = Fresh.var()
    in
      (new_y, freshSpecial (y, new_y, t1))
    end;     *)