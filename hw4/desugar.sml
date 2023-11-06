structure Desugar : sig

  val desugar : Sugary.term -> ULC.term

end = struct

  structure S = Sugary
  structure U = ULC

  (* 1 = ULC.Lam("s", ULC.Lam("z", ULC.App(ULC.Var("s"), ULC.Var("z")))) *)
  fun inToChurchNonZero (term, num) =
    if num = 0 then term
    else U.App (U.Var "s", inToChurchNonZero (term, num - 1))

  fun desugar input = 
    let
      val baseTerm = U.App (U.Var "s", U.Var "z")
      val zero = U.Lam("s", U.Lam("z", U.Var("z")))
      val tru = U.Lam("t", U.Lam("f", U.Var("t")))
      val fls = U.Lam("t", U.Lam("f", U.Var("f")))
      (* fun isZero n = U.Lam(Fresh.var(), U.App(U.App(n, U.Lam("x", fls)), tru)); *)
      fun isZero n = S.Eq(n, S.Nat(0))
      fun predChurch () = U.Lam("n", U.Lam("f", U.Lam("x", 
        U.App(
          U.App(
            U.App(U.Var "n", 
              U.Lam("g", U.Lam("h", U.App(U.Var "h", U.App(U.Var "g", U.Var "f"))))),
            U.Lam("u", U.Var "x")),
          U.Lam("u", U.Var "u")))))
    in
      case input of 
        (*simple*)
          S.True => tru (*take true return true*)
        | S.False => fls (*take true return false*)
        | S.Nat(num) =>
            if num = 0 then
              zero
            else
              U.Lam("s", U.Lam("z", inToChurchNonZero (baseTerm, num-1)))
        | S.Unit => U.Lam("x", U.Var("x"))

        (*calculation*)

        | S.Add (a, b) => 
          U.Lam("a", U.Lam("b", 
              U.Lam("s", 
                U.Lam("z", U.App(U.App(desugar a, U.Var("s")), U.App(U.App(desugar b, U.Var("s")), U.Var("z")))
                ))))
        | S.Mul (a, b) => 
          U.Lam("a",
            U.Lam("b",
              U.App(
                U.App(desugar a, U.Lam("x", U.App(U.App(desugar (S.Add (b, S.Var "x")), U.Var "s"), U.Var "z"))),  (* This is essentially (plus n) *)  
                desugar (S.Nat(0))
              )
            )
          )
        | S.Subtract (a, b) =>
          let
            val pred = 
            U.Lam("n",U.Lam("f",U.Lam("x", 
              U.App(U.App
                (U.App
                  (U.Var("n"),U.Lam("g", U.Lam("h", U.App(U.Var("h"), U.App(U.Var("g"), U.Var("f")))))), 
                  U.Lam("u", U.Var("x"))
                ),
                U.Lam("u", U.Var("u")))
              )))
              (* pred = λn.λf.λx.[[[n (λg.λh.h (g f))] (λu.x)] (λu.u)] *)
          in
            U.Lam("a", U.Lam("b", U.App( U.App(desugar(b), pred), desugar(a) ) ))
          end
        | S.Pow (x, n) => 
          U.Lam("x", U.Lam("n", U.App(desugar(x), desugar(n))))

        (*compare*)

        | S.Greater(m, n) =>
          desugar (S.Not (S.Less(m, n)))
        | S.GreaterEq(m, n) =>
          desugar (S.LessEq(n, m))
        | S.Less(m, n) =>
          desugar (S.Not( isZero(S.Subtract(m, n))) )
        | S.LessEq(m, n) => 
          desugar (S.Or(S.Less(m, n), S.Eq(m, n)))
        | S.Eq(m, n) => 
          let
            val not_less_m_n = S.Not(S.Less(m, n))
            val not_less_n_m = S.Not(S.Less(n, m))
          in
            desugar (S.And(not_less_m_n, not_less_n_m))
          end
        (*condition*)

        | S.Not (x) => 
            U.Lam("x", U.App(U.App(desugar(x), desugar(S.False)), desugar S.True))
        | S.And(a, b) => 
            U.Lam("a", U.Lam("b", U.App(U.App (desugar a, desugar b), desugar a)))
        | S.Or(a, b) => 
            U.Lam("a", U.Lam("b", U.App(U.App(desugar a, desugar a), desugar b)))
        | S.Xor(a, b) =>
          let
            fun nand (x, y) = S.Not(S.And(x, y))
            val term = nand((nand(a, nand(a, b))), (nand(b, nand(a, b))))
          in
            desugar term
          end
        | S.Cond(a, b, c) => 
            U.App(U.App(desugar(a), desugar(b)), desugar(c))

        (*others*)
        | S.Pair (a, b) => 
            U.Lam("a", U.Lam("b", U.Lam( "f", U.App( U.App(U.Var("f"), desugar a), desugar b ) )))
        | S.First (pair) => 
            U.Lam("p", U.App( desugar(pair), desugar(S.True)))
        | S.Second (pair) => 
            U.Lam("p", U.App( desugar(pair), desugar(S.False)))
        | S.Var (s) => U.Var(s)
        | S.Let(s, t1, t2) => U.App (U.Lam (s, desugar t2), desugar t1)
        (* for existing U.val *)
          

    end
end
(* λx. (λy. ((x y) x)) *)

(* IsZero = λn.( (n (λx. false)) true) *)


(* Logical and (AND or λp.λq.p q p).
Logical or (OR or λp.λq.p p q).
Logical not (NOT or λp.λa.λb.p b a). *)
(* plus= λm. λn. λs. λz.ms(nsz); *)
(* times = λm. λn. m (plus n) c0; *)
(* PAIR = λa.( λb.(λf.( (f a) b) ) ) *)
(* ((f a) b) is f(a, b) *)
(* FST = λp.p tru *)
(* SND = λp.p fls *)

(* λx. (λy. ((x y) x)) *)
(* t ::= x      // variables
    | (t1 t2)  // applications, apply function t1 to t2, t1(t2)
    | [x t]  // λx.t, abstractions, lam, DOT take "x" RETURN t, λx.t

This term grammar maps directly into the following SML datatype.

datatype term
  = Var of string
  | App of term * term
  | Lam of string * term *)
