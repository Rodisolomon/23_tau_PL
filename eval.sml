structure Eval : sig

  val isV  : AST.term -> bool
  val isNV : AST.term -> bool
  val step : AST.term -> AST.term option
  val eval : AST.term -> AST.term list
				  
end = struct

  structure A = AST

  fun isNV (AST.Zero) = true
    | isNV (AST.Succ t) = isNV t
    | isNV _ = false;


  fun isV x = 
		(x=A.True) orelse (x=A.False) orelse (isNV x);

  fun step input = 
    let
      fun handlePred(others) = 
      (* 1.2 - 1.4 *)
        case others of
            A.Zero => SOME A.Zero
          | A.Succ others' => 
            if isNV others' then
              SOME others'
            else 
              NONE
          | A.Pred others' =>
              (case step others' of 
                SOME token => SOME (A.Pred token)
              | _ => NONE)
          | _ => NONE;
      fun handleAdd(tokA, tokB) = 
      (* 2.1 - 2.3 *)
        case (tokA, tokB) of
            (A.Zero, tokB) => SOME tokB
          | (A.Succ others, tokB) =>
            if isNV others then
              SOME (A.Add (others, A.Succ(tokB)))
            else
              (case step others of
                SOME tokA' => SOME (A.Add (A.Succ tokA', tokB))
              | _ => NONE)
        (* 2.3 *)
          | (_, _) =>
              (case step tokA of
                SOME tokA' => SOME (A.Add (tokA', tokB))
              | _ => NONE);
      (* SUBTRACT *)
      fun isSucc(tok) =
          case tok of
              A.Succ _ => true
            | _ => false;
      fun extractSucc(tok) = 
          (* only been called when tok is Succ sth*)
          case tok of 
              A.Succ value => value
            | _ => tok;
      fun fourOneFourTwo(tokA, tokB) =
        case step tokB of
          SOME tokB' =>
            if isV tokA then
                SOME (A.Subtract (tokA, tokB'))
            else
                (case step tokA of 
                    SOME tokA' => SOME (A.Subtract (tokA', tokB))
                  | _ => NONE)
          | _ => NONE;
      fun handleSubtr(tokA, tokB) = 
      (* 3.1-3.3, 4.1-4.2 *)
        if tokA = A.Zero andalso isNV tokB then
          SOME A.Zero
        else if tokB = A.Zero andalso isNV tokA then
            SOME tokA
        else if (isSucc tokA) andalso (isSucc tokB) then
          let
            val othersA = extractSucc tokA
            val othersB = extractSucc tokB
          in
            if (isNV othersA) andalso (isNV othersB) then
              SOME (A.Subtract (othersA, othersB))
            else
              fourOneFourTwo(tokA, tokB)
          end
        else
          fourOneFourTwo(tokA, tokB);

      fun SixOneSixTwo(tokA, tokB) = 
          case step tokB of
            SOME tokB' =>
              if isV tokA then
                  SOME (A.Less (tokA, tokB'))
              else
                  (case step tokA of 
                      SOME tokA' => SOME (A.Less (tokA', tokB))
                    | _ => NONE)
            | _ => NONE;

      fun handleLess(tokA, tokB) =
      (* 5.1-5.4, 6.1-6.2 *)
        if tokA = A.Zero andalso tokB = A.Zero then
          SOME A.False
        (* 5.3 *)
        else if tokB = A.Zero andalso isNV tokA then
          SOME A.True
        (* 5.2 *)
        else if tokA = A.Zero andalso isSucc tokB then
          let
            val othersB = extractSucc tokB
          in
            if isNV othersB then
              SOME A.True
            else
              SixOneSixTwo(tokA, tokB)
          end
        (* 5.4 *)
        else if (isSucc tokA) andalso (isSucc tokB) then
          let
            val othersA = extractSucc tokA
            val othersB = extractSucc tokB
          in
            if (isNV othersA) andalso (isNV othersB) then
              SOME (A.Less (othersA, othersB))
            else
              SixOneSixTwo(tokA, tokB)
          end
        else 
          SixOneSixTwo(tokA, tokB);

    fun eightOneEightTwo(tokA, tokB) = 
        case step tokB of
          SOME tokB' =>
            if isV tokA then
                SOME (A.Greater (tokA, tokB'))
            else
                (case step tokA of 
                    SOME tokA' => SOME (A.Greater (tokA', tokB))
                  | _ => NONE)
          | _ => NONE;
      fun handleGreater(tokA, tokB) = 
        (* 7.1, 7.2 *)
          if tokA = A.Zero andalso isNV tokB then
            SOME (A.False)
          else if tokB = A.Zero andalso isSucc tokA then 
            let
              val othersA = extractSucc tokA
            in
              if isNV othersA then
                SOME (A.True)
              else
                SixOneSixTwo(tokA, tokB)
            end
        (* 7.3 *)
          else if (isSucc tokA) andalso (isSucc tokB) then
            let
              val othersA = extractSucc tokA
              val othersB = extractSucc tokB
            in
              if (isNV othersA) andalso (isNV othersB) then
                SOME (A.Greater (othersA, othersB))
              else
                eightOneEightTwo(tokA, tokB)
            end
          else
            eightOneEightTwo(tokA, tokB);

      fun handleCond(tokA, tokB, tokC) = 
    in
      case input of 
      (* 1.1 *)
          A.Succ others =>
            (case step others of 
              SOME token => SOME (A.Succ token)
            | _ => NONE)
      (* 1.X *)
        | A.Pred others => handlePred(others)
      (* 2.X *)
        | A.Add (tokA, tokB) => handleAdd(tokA, tokB)
      (* 3.X+4.X *)
        | A.Subtract (tokA, tokB) => handleSubtr(tokA, tokB)
      (* 5.X + 6.X *)
        | A.Less (tokA, tokB) => handleLess(tokA, tokB)
      (* 7.1, 7.2, 7.3 *)
        | A.Greater (tokA, tokB) => handleGreater(tokA, tokB)
      (* 9.1 and 9.2 *)
        | A.And (A.True, others) =>
          SOME (others)
        | A.And (A.False, others) =>
          SOME (A.False)
      (* 9.3 *)
        | A.And (tok1, tok2) =>
            (case step tok1 of 
              SOME tok1' => SOME (A.And (tok1', tok2))
            | _ => NONE)
      (* 10.1 and 10.2 *)
        | A.Or (A.True, others) =>
          SOME (A.True)
        | A.Or (A.False, others) =>
          SOME (others)
      (* 10.3 *)
        | A.Or (tok1, tok2) =>
            (case step tok1 of 
              SOME tok1' => SOME (A.Or (tok1', tok2))
            | _ => NONE)
      (* 11.X *)
        | _ => NONE
    end;

  fun printTerm(term) =
    case term of
        A.Zero => print "Zero"
      | A.True => print "True"
      | A.False => print "False"
      | A.Succ t => (print "Succ "; printTerm t)
      | A.Pred t => (print "Pred "; printTerm t)
      | A.Add (t1, t2) => (print "(" ; printTerm t1; print " + "; printTerm t2; print ")\n")
      | A.Subtract (t1, t2) => (print "("; printTerm t1; print " - "; printTerm t2; print ")\n")
      | A.Less (t1, t2) => (print "("; printTerm t1; print " < "; printTerm t2; print ")\n")
      | A.Greater (t1, t2) => (print "("; printTerm t1; print " > "; printTerm t2; print ")\n")
      | A.And (t1, t2) => (print "("; printTerm t1; print " AND "; printTerm t2; print ")\n")
      | A.Or (t1, t2) => (print "("; printTerm t1; print " OR "; printTerm t2; print ")\n")
      | A.Cond (t1, t2, t3) => (print "("; print "IF "; printTerm t1; print " THEN "; printTerm t2; print " ELSE "; printTerm t3; print ")\n");

  fun eval input = 
    let 
      open AST;
      fun processValue(acc: A.term list): A.term list = 
        case step (hd acc) of
            SOME value => (
                printTerm value;
              processValue(value::acc))
          | NONE => List.rev acc
    in
      processValue [input]
    end;
	 
end



        (* | A.Subtract (tokA, tokB) => handleSubtr(tokA, tokB)
        | A.Subtract (A.Zero, others) => 
          if isNV others then
            SOME A.Zero
          else
            NONE
      (* 3.2 *)
        | A.Subtract (others, A.Zero) => 
          if isNV others then
            SOME others
          else
            NONE
      (* 3.3 *)
        | A.Subtract (A.Succ others, A.Succ othersB) =>
          if (isNV others) andalso (isNV othersB) then
            SOME (A.Subtract (others, othersB))
          else
            NONE
      (* 4.1 and 4.2 *)
        | A.Subtract(tok1, tok2) =>
          (case step tok2 of
            SOME tok2' =>
              if isV tok1 then
                  SOME (A.Subtract (tok1, tok2'))
              else
                  (case step tok1 of 
                      SOME tok1' => SOME (A.Subtract (tok1', tok2))
                    | _ => NONE)
            | _ => NONE) *)