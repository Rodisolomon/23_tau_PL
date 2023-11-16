structure Eval : sig

  val eval : L23RR.term -> L23RR.term
	    
end = struct

  structure L = L23RR
  structure S = Subst

  fun eval (L.Int n) = L.Int n
    | eval (L.True) = L.True
    | eval (L.False) = L.False
    | eval (L.Unit) = L.Unit
    | eval (L.Lam (x, tau, t)) = L.Lam (x, tau, t)
    | eval (L.App (t1, t2)) = 
        (case (eval t1, eval t2) of
           (L.Lam (x, _, t), v2) => eval (S.subst (x, v2, t))
         | _ => raise Fail "Application to a non-lambda")
    | eval (L.Fix t1) = 
        (case eval t1 of
           L.Lam (f, tau, t) => eval (S.subst (f, L.Fix (L.Lam (f, tau, t)), t))
         | _ => raise Fail "Fixpoint of non-lambda")
    | eval (L.Let (x, t1, t2)) = 
        let 
          val v1 = eval t1 
        in 
          eval (S.subst (x, v1, t2)) 
        end
    | eval (L.Cond (t1, t2, t3)) = 
        (case eval t1 of
           L.True => eval t2
         | L.False => eval t3
         | _ => raise Fail "Condition is not a boolean")
    | eval (L.Add (t1, t2)) = 
        (case (eval t1, eval t2) of
           (L.Int n1, L.Int n2) => L.Int (n1 + n2)
         | _ => raise Fail "Addition of non-integers")
    | eval (L.Sub (t1, t2)) = 
        (case (eval t1, eval t2) of
           (L.Int n1, L.Int n2) => L.Int (n1 - n2)
         | _ => raise Fail "subtraction of non-integers")
    | eval (L.Mul (t1, t2)) = 
        (case (eval t1, eval t2) of
           (L.Int n1, L.Int n2) => L.Int (n1 * n2)
         | _ => raise Fail "multiplication of non-integers")
    | eval (L.LessThan (t1, t2)) = 
        (case (eval t1, eval t2) of
          (L.Int n1, L.Int n2) => 
            let
              val res = n1-n2
            in
              if res < 0 then L.True else L.False
            end
          | _ => raise Fail "multiplication of non-integers")    
    | eval (L.Eq (t1, t2)) = 
        (case (eval t1, eval t2) of
          (L.Int n1, L.Int n2) => 
            let
              val res = n1-n2
            in
              if res = 0 then L.True else L.False
            end
          | _ => raise Fail "multiplication of non-integers")     
    | eval (L.Not t1) = 
        (case eval t1 of
           L.True => L.False
         | L.False => L.True
         | _ => raise Fail "Negation of non-boolean")
    | eval (L.Record fields) = 
        L.Record (List.map (fn (l, t) => (l, eval t)) fields)
    | eval (L.Select (label, t1)) = 
        (case eval t1 of
           L.Record fields =>
             (case List.find (fn (l, _) => l = label) fields of
                SOME (_, v) => v
              | NONE => raise Fail ("Label " ^ label ^ " not found in record"))
         | _ => raise Fail "Selection from non-record type")
		 
end
