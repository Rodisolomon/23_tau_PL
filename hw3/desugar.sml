structure Desugar : sig

  val desugar : Sweetl.term -> ULC.term

end = struct
  struct S = Sweetl
  struct U = ULC

  fun intToChurch 0 = U.Lam("f", U.Lam("x", U.Var("x")))
    | intToChurch n = U.Lam("f", U.Lam("x", U.App(U.Var("f"), intToChurch (n - 1))));

  fun desugar input = 
    case input of 
        S.Var(str) => U.Var(str)
      | S.Lam(str, term) => U.Lam(str, desugar(term))
      | S.App(term1, term2) => U.App(desugar(term1), desugar(term2))
      | S.Tru => U.Lam("x", U.Lam("y", Var("x")))
      | S.Fls => U.Lam("x", U.Lam("y", Var("y")))
      | S.Nat(num) =>
          if n = 0 then
            U.Lam("f", U.Lam("x", U.Var("x")))
          else
            intToChurch num
      | S.ID(str) => U.Lam("x", U.Var("x"))


end
