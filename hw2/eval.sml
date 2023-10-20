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
      fun printTerm(term) =
        case term of
            D.Zero => print "Zero?"
          | D.Succ t => (print "Succ "; printTerm t)
          | D.First t => (print "First "; printTerm t)
          | D.Second t => (print "Second "; printTerm t)
          | D.Pair (t1, t2) => (print "(" ; printTerm t1; print ", "; printTerm t2; print ")")
          | D.Add (t1, t2) => (print "(" ; printTerm t1; print " + "; printTerm t2; print ")")
          | D.Less (t1, t2) => (print "(" ; printTerm t1; print " < "; printTerm t2; print ")")
          | D.Subtract (t1, t2) => (print "("; printTerm t1; print " - "; printTerm t2; print ")")
          | D.Eq (t1, t2) => (print "("; printTerm t1; print " = "; printTerm t2; print ")")
          | D.Cond (t1, t2, t3) => (print "("; print "IF "; printTerm t1; print " THEN "; printTerm t2; print " ELSE "; printTerm t3; print ")");
          
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
                SOME tokA' => SOME (D.Add (tokA', tokB))
              | _ => NONE)
          | _ => 
            (case step tokA of
                SOME tokA' => SOME (D.Add (tokA', tokB))
              | _ => NONE);
      fun threeOneThreeTwo(tokA, tokB) =
        case step tokB of
          SOME tokB' =>
            if isV tokA then
                SOME (D.Subtract (tokA, tokB'))
            else
                (case step tokA of 
                    SOME tokA' => SOME (D.Add (tokA', tokB))
                  | _ => NONE)
          | _ =>  (case step tokA of 
                SOME tokA' => SOME (D.Add (tokA', tokB))
              | _ => NONE);
      fun handleSubtr(tokA, tokB) = 
      (* 2.1-2.3, 3.1-3.2 *)
        if tokA = D.Zero andalso isV tokB then
          SOME D.Zero
        else if isV tokA andalso tokB = D.Zero then
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

      fun isPair(tok) =
        case tok of
            D.Pair _ => true
          | _ => false;

      fun extractPair(tok) = 
          (* only been called when tok is pair sth*)
          case tok of 
              D.Pair (valA, valB) => (valA, valB)
            | _ => (tok, tok); (* neverreachthispoint *)

      fun EightOneEightTwo(tokA, tokB) = 
          case step tokB of
            SOME tokB' =>
              if isV tokA then
                  SOME (D.Eq (tokA, tokB'))
              else
                  (case step tokA of 
                      SOME tokA' => SOME (D.Eq (tokA', tokB))
                    | _ => NONE)
            | _ => (case step tokA of 
                    SOME tokA' => SOME (D.Eq (tokA', tokB))
                  | _ => NONE);

      fun handleEq(tokA, tokB) =
      (* 6.1-6.4, 7.1, 8.1-8.2 *)
        if tokA = D.Zero andalso tokB = D.Zero then
          SOME (D.Succ (D.Zero))
        (* 6.3 *)
        else if tokB = D.Zero andalso isSucc tokA then
          let
            val othersA = extractSucc tokA
          in
            if isV othersA then
              SOME D.Zero
            else
              EightOneEightTwo(tokA, tokB)
          end        
        (* 6.2 *)
        else if tokA = D.Zero andalso isSucc tokB then
          let
            val othersB = extractSucc tokB
          in
            if isV othersB then
              SOME D.Zero
            else
              EightOneEightTwo(tokA, tokB)
          end
        (* 6.4 *)
        else if (isSucc tokA) andalso (isSucc tokB) then
          let
            val othersA = extractSucc tokA
            val othersB = extractSucc tokB
          in
            if (isV othersA) andalso (isV othersB) then
              SOME (D.Eq (othersA, othersB))
            else
              EightOneEightTwo(tokA, tokB)
          end
        (* 7.1 *)
        else if (isPair tokA) andalso (isPair tokB) then
          let
            val (v1, v2) = extractPair tokA
            val (v3, v4) = extractPair tokB
            (* val _ = print "\nis pair\n"
            val _ = printTerm v1
            val result = if isV tokA then "true" else "false";
            val _ = print result *)
          in
            if (isV tokA) andalso (isV tokB) then
              SOME ( D.Cond(D.Eq (v1, v3), D.Eq (v2, v4), D.Zero) )
            else
              EightOneEightTwo(tokA, tokB) 
          end
        else 
          EightOneEightTwo(tokA, tokB);

      fun handleCond(tokA, tokB, tokC) = 
        if tokA = D.Succ (D.Zero) then
          SOME (tokB)
        else if tokA = D.Zero then
          SOME (tokC)
        else
          case step tokA of 
              SOME tokA' => SOME (D.Cond (tokA', tokB, tokC))
            | _ => NONE;

      (* val _ = print "\n"
      val _ = printTerm input *)
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
        | D.Eq (tokA, tokB) => handleEq(tokA, tokB)
        | D.Cond (tokA, tokB, tokC) => handleCond(tokA, tokB, tokC)

        | D.Pair (tokA, tokB) => 
          if isV tokA then
            (case step tokB of
               SOME tokB' => SOME (D.Pair (tokA, tokB'))
              | _ => NONE)
          else
            (case step tokA of
               SOME tokA' => SOME (D.Pair (tokA', tokB))
              | _ => NONE
              )             

        | D.First (tok) => 
          (case tok of
              D.Pair (tokA, tokB) =>
                if isV tokA andalso isV tokB then
                  SOME tokA
                else
                (case step tok of 
                  SOME tok' => SOME (D.First (tok'))
                  | _ => NONE)
              | _ =>
              (case step tok of 
                SOME tok' => SOME (D.First (tok'))
                | _ => NONE
              )
          )
        | D.Second (tok) => 
          (case tok of
              D.Pair (tokA, tokB) =>
                if isV tokA andalso isV tokB then
                  SOME tokB
                else
                (case step tok of 
                  SOME tok' => SOME (D.First (tok'))
                  | _ => NONE)
              | _ =>
              (case step tok of 
                SOME tok' => SOME (D.First (tok'))
                | _ => NONE)

          )
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

    fun printTerm(term) =
      case term of
          D.Zero => print "Zero"
        | D.Succ t => (print "Succ "; printTerm t)
        | D.First t => (print "First "; printTerm t)
        | D.Second t => (print "Second "; printTerm t)
        | D.Pair (t1, t2) => (print "(" ; printTerm t1; print ", "; printTerm t2; print ")")
        | D.Add (t1, t2) => (print "(" ; printTerm t1; print " + "; printTerm t2; print ")")
        | D.Less (t1, t2) => (print "(" ; printTerm t1; print " < "; printTerm t2; print ")")
        | D.Subtract (t1, t2) => (print "("; printTerm t1; print " - "; printTerm t2; print ")")
        | D.Eq (t1, t2) => (print "("; printTerm t1; print " == "; printTerm t2; print ")")
        | D.Cond (t1, t2, t3) => (print "("; print "IF "; printTerm t1; print " THEN "; printTerm t2; print " ELSE "; printTerm t3; print ")");

  fun lastElement([]) = raise Fail "Empty list"  (* Raise exception when the list is empty *)
    | lastElement([x]) =  x
    | lastElement(x::xs) = lastElement(xs);
  fun result input = 
    let
      val result_lst = eval input
      val res = lastElement result_lst
      (* val _ = print "\nfinal result \n"
      val _ = printTerm res *)
    in
      case isV res of 
          false => Stuck res
        | _ => Value res
    end;

end
