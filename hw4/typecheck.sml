structure TypeCheck : sig
	    
  val typeof : TypeEnv.env * Sugary.term -> Type.typ
	    
end = struct

  structure S = Sugary
  structure T = Type
  structure E = TypeEnv

  fun typeof (env, term) = 
    let 
      fun handleCalc (env, A, B) = 
        case (A, B) of
            (S.Nat _, S.Nat _) => Type.Nat
          | _ => 
            (case (typeof (env, A), typeof (env, B)) of
                (T.Nat, T.Nat) => T.Nat
              | _ => raise Fail "wrong type for calculation operator");

      fun handleRelational (env, A, B) = 
        (case (A, B) of
            (S.Nat _, S.Nat _) => T.Bool
          | _ => 
            (case (typeof (env, A), typeof (env, B)) of
                (T.Nat, T.Nat) => T.Bool
              | _ => raise Fail "wrong type for relational operator")
        );

      fun processAdd (env, new_key, new_type) = 
        let
          val _ = E.extend (env, new_key, new_type)
        in
          new_type
        end;

    in
      case (env, term) of
          (_, S.Nat num) => T.Nat
        | (_, S.True) => T.Bool
        | (_, S.False) => T.Bool
        | (_, S.Unit) => T.Unit
        | (_, S.Add (A, B)) => handleCalc (env, A, B)
        | (_, S.Subtract (A, B)) => handleCalc (env, A, B)
        | (_, S.Mul (A, B)) => handleCalc (env, A, B)
        | (_, S.Pow (A, B)) => handleCalc (env, A, B)
        | (_, S.Less (A, B)) => handleRelational (env, A, B)
        | (_, S.Greater (A, B)) => handleRelational (env, A, B)
        | (_, S.LessEq (A, B)) => handleRelational (env, A, B)
        | (_, S.GreaterEq (A, B)) => handleRelational (env, A, B)
        | (_, S.Not (A)) =>
          (case typeof (env, A) of
              T.Bool => T.Bool
            | _ => raise Fail "wrong type for not operator"
          )
        | (_, S.And (A, B)) => 
          (case (typeof (env, A), typeof (env, B)) of
              (T.Bool, T.Bool) => T.Bool
            | _ => raise Fail "wrong type for And operator"
          )
        | (_, S.Or (A, B)) =>
          (case (typeof (env, A), typeof (env, B)) of
              (T.Bool, T.Bool) => T.Bool
            | _ => raise Fail "Wrong type for Or operator"
          )
        | (_, S.Xor (A, B)) =>
          (case (typeof (env, A), typeof (env, B)) of
              (T.Bool, T.Bool) => T.Bool
            | _ => raise Fail "wrong type for Xor operator"   
          ) 
        | (_, S.Eq (A, B)) => 
          (case (typeof (env, A), typeof (env, B)) of
              (typeA, typeB) => 
                if typeA = typeB then
                  T.Bool
                else
                  raise Fail "dif type for Cond operator"
          )
        | (_, S.Cond (A, B, C)) =>
          (case (typeof (env, A), typeof (env, B), typeof (env, C)) of
            (T.Bool, typeB, typeC) => 
                if typeB = typeC then
                  typeB
                else
                  raise Fail "dif type for Cond operator"
            | _ => raise Fail "wrong type for Cond operator"
          )
        | (_, S.Pair(A, B)) => 
          (case (typeof (env, A), typeof (env, B)) of
            (_, _) => T.Product (typeof (env, A), typeof (env, B))
          )
        | (_, S.First(A)) =>
          (case (typeof (env, A)) of
            T.Product (firstToken , _) => firstToken
            | _ => raise Fail "dif type for First operator"
          )       
        | (_, S.Second(A)) =>
          (case (typeof (env, A)) of
            T.Product (_ , secondToken) => secondToken
            | _ => raise Fail "dif type for Second operator"
          )
      | (_, S.Var(s)) =>
        (case E.lookup (env, s) of
            SOME tau => tau
          | NONE => raise Fail "this type doesn't exist"
        ) 
      | (_, S.Let(s, t1, t2)) =>
        (case typeof (env, t1) of
            tau => 
              (case typeof (env, t2) of
                  type2 =>  processAdd (env, s, type2) 
              )
        ) 

    end




end
