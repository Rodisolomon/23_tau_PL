structure TypeCheck : sig
  val typeof : Sugary.term -> Type.typ
end = struct
  structure S = Sugary
  structure T = Type


  fun typeof input = 
    let 
      fun handleCalc (A, B) = 
        case (A, B) of
            (S.Nat _, S.Nat _) => Type.Nat
          | _ => 
            (case (typeof A, typeof B) of
                (T.Nat, T.Nat) => T.Nat
              | _ => raise Fail "wrong type for calculation operator")
      fun handleRelational (A, B) = 
        (case (A, B) of
            (S.Nat _, S.Nat _) => T.Bool
          | _ => 
            (case (typeof A, typeof B) of
                (T.Nat, T.Nat) => T.Bool
              | _ => raise Fail "wrong type for relational operator")
        )
    in
      case input of 
          S.Nat num => T.Nat
        | S.True => T.Bool
        | S.False => T.Bool
        | S.Unit => T.Unit
        | S.Add (A, B) => handleCalc (A, B)
        | S.Subtract (A, B) => handleCalc (A, B) 
        | S.Less (A, B) => handleRelational (A, B)
        | S.LessEq (A, B) => handleRelational (A, B)
        | S.Greater (A, B) => handleRelational (A, B) 
        | S.GreaterEq (A, B) => handleRelational (A, B)
        | S.Eq (A, B) => 
          (case (typeof A, typeof B) of
              (typeA, typeB) => 
                if typeA = typeB then
                  T.Bool
                else
                  raise Fail "dif type for Cond operator"
          )
        | S.Not (A) =>
          (case typeof A of
              T.Bool => T.Bool
            | _ => raise Fail "wrong type for not operator"
          )
        | S.And (A, B) => 
          (case (typeof A, typeof B) of
              (T.Bool, T.Bool) => T.Bool
            | _ => raise Fail "wrong type for And operator"
          )
        | S.Or (A, B) =>
          (case (typeof A, typeof B) of
              (T.Bool, T.Bool) => T.Bool
            | _ => raise Fail "Wrong type for Or operator"
          )
        | S.Xor (A, B) =>
          (case (typeof A, typeof B) of
              (T.Bool, T.Bool) => T.Bool
            | _ => raise Fail "wrong type for Xor operator"   
          ) 
        | S.Cond (A, B, C) =>
          (case (typeof A, typeof B, typeof C) of
            (T.Bool, typeB, typeC) => 
                if typeB = typeC then
                  typeB
                else
                  raise Fail "dif type for Cond operator"
            | _ => raise Fail "wrong type for Cond operator"
          )
        | S.Pair(A, B) => 
          (case (typeof A, typeof B) of
            (_, _) => T.Product (typeof A, typeof B)
          )
        | S.First(A) =>
          (case (typeof A) of
            T.Product (firstToken , _) => firstToken
            | _ => raise Fail "dif type for First operator"
          )       
        | S.Second(A) =>
          (case (typeof A) of
            T.Product (_ , secondToken) => secondToken
            | _ => raise Fail "dif type for Second operator"
          )
    end;

end
