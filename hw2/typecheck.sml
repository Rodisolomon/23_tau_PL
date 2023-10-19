structure TypeCheck : sig

  val typeof : Sugary.term -> Type.typ

end = struct

  structure S = Sugary
  structure T = Type
  fun handleCalc (A, B) = 
      case (A, B) of
        (S.Nat _, S.Nat _) => T.Nat
      | _ => raise Fail "wrong type for relational operator"
  fun handleRelational (A, B) = 
      case (A, B) of
        (S.Nat _, S.Nat _) => T.Bool
      | _ => raise Fail "wrong type for relational operator"

  fun typeof input = 
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
          (T.Bool, T.Bool) => T.Bool
        | _ => T.Bool
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
         (T.Bool, _, _) => typeof B
        | _ => raise Fail "wrong type for Cond operator"
      )
    | S.Pair(A, B) => 
      (case (typeof A, typeof B) of
         (_, _) => T.Product (typeof A, typeof B)
      )
    | S.First(A) =>
      (case (typeof A) of
         T.Product (firstToken , _) => firstToken
        | _ => raise Fail "expecting T.Product"
      )       
    | S.Second(A) =>
      (case (typeof A) of
         T.Product (_ , secondToken) => secondToken
        | _ => raise Fail "expecting T.Product"
      );

end
