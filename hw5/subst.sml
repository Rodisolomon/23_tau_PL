structure Subst : sig

  val subst : string * L23RR.term * L23RR.term -> L23RR.term
	    
end = struct

  structure L = L23RR

(* note: We will do without VarSet/FV this time. *)
(* This is because free variables are effectively banished by the typechecker. *)
(* That is, we shouldn't have them when we get to evaluation. *)
(* t is here to replace x in s *)

  fun subst (x, s, t) =
    case t of
        L.Var y => if x = y then s else t
      | L.Lam (y, typ, t1) => if x = y then L.Lam(x, typ, t1) else L.Lam (y, typ, subst (x, s, t1))
      | L.App (t1, t2) => L.App (subst (x, s, t1), subst (x, s, t2))
      | L.Let (y, t1, t2) => L.Let (y, subst (x, s, t1), if x = y then t2 else subst (x, s, t2))
      | L.Cond (t1, t2, t3) => L.Cond (subst (x, s, t1), subst (x, s, t2), subst (x, s, t3))
      | L.Add (t1, t2) => L.Add (subst (x, s, t1), subst (x, s, t2))
      | L.Sub (t1, t2) => L.Sub (subst (x, s, t1), subst (x, s, t2))
      | L.Mul (t1, t2) => L.Mul (subst (x, s, t1), subst (x, s, t2))
      | L.Eq (t1, t2) => L.Eq (subst (x, s, t1), subst (x, s, t2))
      | L.LessThan (t1, t2) => L.LessThan (subst (x, s, t1), subst (x, s, t2))
      | L.Not t1 => L.Not (subst (x, s, t1))
      | L.Record pairs => L.Record (List.map (fn (l, t1) => (l, subst (x, s, t1))) pairs)
      | L.Select (l, t1) => L.Select (l, subst (x, s, t1))
      | _ => t
      
end
