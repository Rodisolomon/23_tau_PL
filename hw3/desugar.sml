structure Desugar : sig

  val desugar : Sweetl.term -> ULC.term

end = struct

  structure S = Sweetl
  structure U = ULC

  fun intToChurch 0 = U.Lam("s", U.Lam("z", U.Var("z")))
    | intToChurch n = U.Lam("s", U.Lam("z", U.App(U.Var("s"), intToChurch (n - 1))));

  fun desugar input = 
    case input of 
        S.Var(str) => U.Var(str)
      | S.Lam(str, term) => U.Lam(str, desugar(term))
      | S.App(term1, term2) => U.App(desugar(term1), desugar(term2))
      | S.Tru => U.Lam("t", U.Lam("f", U.Var("t")))
      | S.Fls => U.Lam("t", U.Lam("f", U.Var("f")))
      | S.Nat(num) =>
          if num = 0 then
            U.Lam("f", U.Lam("x", U.Var("x")))
          else
            intToChurch num
      | S.ID(str) => U.Lam(str, U.Var(str))


end
