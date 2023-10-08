structure Parse : sig

  val next  : Token.token list -> (AST.term * Token.token list) option
  val parse : Token.token list -> AST.term

end = struct

  structure T = Token
  structure A = AST


  fun extractFirstElement xs =
      case xs of
          [] => NONE (* If the list is empty, return NONE *)
        | x :: rest => SOME (x, rest); (* If the list is not empty, return the first element wrapped in SOME *)
  
  (* [[SZ < SSSSZ] || F] *)
  (* eg. [T.LBrack, T.LBrack, T.Z, T.LessThan, T.S, T.Z, T.RBrack, T.DoublePipe, T.F] *)

fun next tokens =
    case tokens of
        [] => NONE (* No more tokens, return NONE *)
      | T.Z :: rest => SOME (A.Zero, rest)
      | T.T :: rest => SOME (A.True, rest)
      | T.F :: rest => SOME (A.False, rest)

      | T.S :: rest =>
          (case next rest of
              SOME (term, tokens') => SOME (A.Succ term, tokens')
            | NONE => raise Fail "Invalid syntax after Succ")
      | T.P :: rest =>
          (case next rest of
              SOME (term, tokens') => SOME (A.Pred term, tokens')
            | NONE => raise Fail "Invalid syntax after Pred")

      | T.LBrack :: T.RBrack :: rest => NONE (* Empty brackets, return NONE *)
      | T.LBrack :: rest =>
        let
            val (term1, tokens1) = case next rest of
                SOME (term, tokens') => (term, tokens')
              | NONE => raise Fail "Invalid syntax for term1"
        in
            (case tokens1 of
                T.Plus :: tokens' =>
                  let
                      val (term2, tokens2) = case next tokens' of
                          SOME (term, tokens') => (term, tokens')
                        | NONE => raise Fail "Invalid syntax for term2"
                  in
                      (case tokens2 of
                        T.RBrack :: tokens'' => SOME (A.Add (term1, term2), tokens'')
                        | _ => raise Fail "Invalid syntax for addition")
                  end
              | T.Minus :: tokens' =>
                  let
                      val (term2, tokens2) = case next tokens' of
                        SOME (term, tokens') => (term, tokens')
                        | NONE => raise Fail "Invalid syntax for term2"
                  in
                      (case tokens2 of
                        T.RBrack :: tokens'' => SOME (A.Subtract (term1, term2), tokens'')
                        | _ => raise Fail "Invalid syntax for subtraction")
                  end
              | T.LessThan :: tokens' =>
                  let
                      val (term2, tokens2) = case next tokens' of
                        SOME (term, tokens') => (term, tokens')
                        | NONE => raise Fail "Invalid syntax for term2"
                  in
                      (case tokens2 of
                        T.RBrack :: tokens'' => SOME (A.Less (term1, term2), tokens'')
                        | _ => raise Fail "Invalid syntax for less than")
                  end
              | T.GreaterThan :: tokens' =>
                  let
                      val (term2, tokens2) = case next tokens' of
                        SOME (term, tokens') => (term, tokens')
                        | NONE => raise Fail "Invalid syntax for term2"
                  in
                      (case tokens2 of
                        T.RBrack :: tokens'' => SOME (A.Greater (term1, term2), tokens'')
                        | _ => raise Fail "Invalid syntax for greater than")
                  end
              | T.DoubleAmpersand :: tokens' =>
                  let
                      val (term2, tokens2) = case next tokens' of
                        SOME (term, tokens') => (term, tokens')
                        | NONE => raise Fail "Invalid syntax for term2"
                  in
                      (case tokens2 of
                        T.RBrack :: tokens'' => SOME (A.And (term1, term2), tokens'')
                        | _ => raise Fail "Invalid syntax for And")
                  end
              | T.DoublePipe :: tokens' =>
                  let
                      val (term2, tokens2) = case next tokens' of
                        SOME (term, tokens') => (term, tokens')
                        | NONE => raise Fail "Invalid syntax for term2"
                  in
                      (case tokens2 of
                        T.RBrack :: tokens'' => SOME (A.Or (term1, term2), tokens'')
                        | _ => raise Fail "Invalid syntax for or")
                  end

              | T.QuestionMark :: tokens' =>
                  let
                      val (term2, tokens2) = case next tokens' of
                        SOME (term, tokens') => (term, tokens')
                        | NONE => raise Fail "Invalid syntax for term2"
                  in
                      (case tokens2 of
                        T.Colon :: tokens'' =>
                            let
                                val (term3, tokens3) = case next tokens'' of
                                  SOME (term2, tokens'') => (term2, tokens'')
                                | NONE => raise Fail "Invalid syntax for term3"
                            in
                              (case tokens3 of
                                T.RBrack :: tokens''' => SOME (A.Cond (term1, term2, term3), tokens''')
                                | _ => raise Fail "Invalid syntax for or")                              
                            end
                        | _ => raise Fail "should be colon"
                      )
                  end

              | _ => raise Fail "Invalid syntax, expected operator after term1"
            )
        end

      | _ => raise Fail "failure"



  fun parse toks = 
		(* This function has to return a single term in the end (it has to be nested though)    *)
    case next toks of
        SOME (term, []) => term
      | _ => raise Fail "Invalid input"
end
