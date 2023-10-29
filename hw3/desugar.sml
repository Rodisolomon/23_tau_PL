structure Desugar : sig

  val desugar : Sweetl.term -> ULC.term

end = struct

  structure S = Sweetl
  structure U = ULC

  fun inToChurchNonZero (term, num) =
    if num = 0 then term
    else U.App (U.Var "s", inToChurchNonZero (term, num - 1))

  fun desugar input = 
    let
      val baseTerm = U.App (U.Var "s", U.Var "z")
    in
      case input of 
          S.Var(str) => U.Var(str)
        | S.Tru => U.Lam("t", U.Lam("f", U.Var("t")))
        | S.Fls => U.Lam("t", U.Lam("f", U.Var("f")))
        | S.ID(str) => U.Lam(str, U.Var(str))
        | S.Lam(str, term) => U.Lam(str, desugar(term))
        | S.App(term1, term2) => U.App(desugar(term1), desugar(term2))
        | S.Nat(num) =>
            if num = 0 then
              U.Lam("s", U.Lam("z", U.Var("z")))
            else
              U.Lam("s", U.Lam("z", inToChurchNonZero (baseTerm, num-1)))
    end


end
