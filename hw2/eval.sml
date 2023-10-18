structure Eval : sig

  val isV  : Desugared.term -> bool
  val step : Desugared.term -> Desugared.term option
  val eval : Desugared.term -> Desugared.term list

  datatype norm
    = Value of Desugared.term
    | Stuck of Desugared.term	    

  val result : Desugared.term -> norm
      
end = struct

  datatype norm
    = Value of Desugared.term
    | Stuck of Desugared.term

  structure D = Desugared

  fun isNV (D.Zero) = true
    | isNV (D.Succ t) = isNV t
    | isNV _ = false;

  fun isV x = 
    case x of
     D.Pair(A, B) =>
     (case (isV A, isV B) of
       (true, true) => true
      | (_, _) => false
     )
    | _ => isNV x;

		 
  fun step input =
    let
      fun isSucc(tok) =
        case tok of
            D.Succ _ => true
          | _ => false;
      fun extractSucc(tok) = 
          (* only been called when tok is Succ sth*)
          case tok of 
              D.Succ value => value
            | _ => tok;

      fun handleAdd(tokA, tokB) = 
      (* 1.2 - 1.4 *)
        case (tokA, tokB) of
            (D.Zero, tokB) => SOME tokB
          | (D.Succ others, tokB) =>
            if isV others then
              SOME (D.Add (others, D.Succ(tokB)))
            else
              (case step others of
                SOME tokA' => SOME (D.Add (D.Succ tokA', tokB))
              | _ => NONE)

      fun threeOneThreeTwo(tokA, tokB) =
        case step tokB of
          SOME tokB' =>
            if isV tokA then
                SOME (D.Subtract (tokA, tokB'))
            else
                (case step tokA of 
                    SOME tokA' => SOME (D.Subtract (tokA', tokB))
                  | _ => NONE)
          | _ =>  (case step tokA of 
                SOME tokA' => SOME (D.Subtract (tokA', tokB))
              | _ => NONE);
      fun handleSubtr(tokA, tokB) = 
      (* 2.1-2.3, 3.1-3.2 *)
        if tokA = D.Zero andalso isV tokB then
          SOME D.Zero
        else if tokB = D.Zero andalso isV tokA then
            SOME tokA
        else if (isSucc tokA) andalso (isSucc tokB) then
          let
            val othersA = extractSucc tokA
            val othersB = extractSucc tokB
          in
            if (isV othersA) andalso (isV othersB) then
              SOME (D.Subtract (othersA, othersB))
            else
              threeOneThreeTwo(tokA, tokB)
          end
        else
          threeOneThreeTwo(tokA, tokB);

      fun fiveOneFiveTwo(tokA, tokB) = 
          case step tokB of
            SOME tokB' =>
              if isV tokA then
                  SOME (D.Less (tokA, tokB'))
              else
                  (case step tokA of 
                      SOME tokA' => SOME (D.Less (tokA', tokB))
                    | _ => NONE)
            | _ => (case step tokA of 
                    SOME tokA' => SOME (D.Less (tokA', tokB))
                  | _ => NONE);

      fun handleLess(tokA, tokB) =
      (* 5.1-5.4, 6.1-6.2 *)
        if tokA = D.Zero andalso tokB = D.Zero then
          SOME D.Zero
        (* 5.3 *)
        else if tokB = D.Zero andalso isV tokA then
          SOME D.Zero
        (* 5.2 *)
        else if tokA = D.Zero andalso isSucc tokB then
          let
            val othersB = extractSucc tokB
          in
            if isV othersB then
              SOME (D.Succ D.Zero)
            else
              fiveOneFiveTwo(tokA, tokB)
          end
        (* 5.4 *)
        else if (isSucc tokA) andalso (isSucc tokB) then
          let
            val othersA = extractSucc tokA
            val othersB = extractSucc tokB
          in
            if (isV othersA) andalso (isV othersB) then
              SOME (D.Less (othersA, othersB))
            else
              fiveOneFiveTwo(tokA, tokB)
          end
        else 
          fiveOneFiveTwo(tokA, tokB);

    in
      case input of 
      (* 1.1 *)
          D.Succ others =>
            (case step others of 
              SOME token => SOME (D.Succ token)
            | _ => NONE)
        | D.Add (tokA, tokB) => handleAdd(tokA, tokB)
        | D.Subtract (tokA, tokB) => handleSubtr(tokA, tokB)
        | D.Less (tokA, tokB) => handleLess(tokA, tokB)

        | _ => NONE
    end;
        
				    
  fun eval t = 
    let
      fun lp t =
	(case step t
	   of SOME t' => t :: lp t'
	    | NONE => [t])
    in
      lp t
    end;

  fun result input = 
    let
      val result_lst = eval input
    in
      case result_lst of
          x :: _ => Value x
        | _ => raise Fail "should not be empty"
    end;

end
