structure Desugar : sig

  val desugar : Sugary.term -> Desugared.term

end = struct
 
  structure S = Sugary
  structure D = Desugared


  (* - rewrite natural numbers to Z, SZ, etc.
  - rewrite true to SZ
  - rewrite false to Z
  - rewrite unit to Z
  - rewrite <=, >, >=, !, &&, ||, ^^ into Desugared terms  *)

  fun desugarNum (num) =
    if num = 0 then 
      D.Zero
    else
      D.Succ (desugarNum(num-1));

  fun desugar input = 
    case input of
        S.True => D.Succ (D.Zero)
      | S.False => D.Zero
      | S.Unit => D.Zero
      | S.Subtract (A, B) => 
        let 
          val des_A = desugar A
          val des_B = desugar B
        in
          D.Subtract (des_A, des_B)
        end
      | S.Add (A, B) => 
        let 
          val des_A = desugar A
          val des_B = desugar B
        in
          D.Add (des_A, des_B)
        end
      | S.Nat num => desugarNum(num)
      | S.Less (A, B) => 
        let 
          val des_A = desugar A
          val des_B = desugar B
        in
          D.Less (des_A, des_B)
        end
      | S.Greater (A, B) => 
        let 
          val desA = desugar A
          val desB = desugar B
        in
          D.Cond(D.Less(desB, desA), D.Succ (D.Zero), D.Zero)
        end
      | S.GreaterEq (A, B) => 
        let 
          val desA = desugar A
          val desB = desugar B
          val greater = desugar (S.Greater (A, B))
          val orr = D.Cond(greater, D.Succ (D.Zero), D.Eq(desA, desB))
        in
          D.Cond(orr, D.Succ (D.Zero), D.Zero)
        end
      | S.LessEq (A, B) => 
        let 
          val desA = desugar A
          val desB = desugar B
          val orr = D.Cond(D.Less(desA, desB), D.Succ (D.Zero), D.Eq(desA, desB))
        in
          D.Cond(orr, D.Succ (D.Zero), D.Zero)
        end
      | S.Not (A) =>
        let 
          val desA = desugar A
        in
          D.Cond(desA, D.Zero, D.Succ (D.Zero))
        end
      | S.And (A, B) =>
        let 
          val desA = desugar A
          val desB = desugar B
        in
          D.Cond(desA, desB, D.Zero) 
        end    
      | S.Or (A, B) => 
        let 
          val desA = desugar A
          val desB = desugar B
        in
          D.Cond(desA, D.Succ (D.Zero), desB)
        end
      | S.Xor (A, B) =>
        let 
          val desA = desugar A
          val desB = desugar B
          val notVal = D.Cond(D.Eq(desA, desB), D.Zero, D.Succ (D.Zero))
        in
          D.Cond(notVal, D.Succ (D.Zero), D.Zero)
        end
      | S.Cond (A, B, C) => 
        let 
          val des_A = desugar A
          val des_B = desugar B
          val des_C = desugar C
        in
          D.Cond (des_A, des_B, des_C)
        end 
      | S.Eq (A, B) => 
        let 
          val des_A = desugar A
          val des_B = desugar B
        in
          D.Eq (des_A, des_B)
        end
      | S.Pair (A, B) => 
        let 
          val des_A = desugar A
          val des_B = desugar B
        in
          D.Pair (des_A, des_B)
        end
      | S.First (A) => 
        let 
          val des_A = desugar A
        in
          D.First (des_A)
        end
      | S.Second (A) => 
        let 
          val des_A = desugar A
        in
          D.Second (des_A)
        end;



      
end
