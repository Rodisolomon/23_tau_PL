structure RecordCheck : sig

(* check for pairwise distinct labels at all levels of record expressions and record types *)
(* also, reject empty records if you encounter them *)

(* raise an exception if the term doesn't pass the check *)

(* otherwise, return the term as is *)

  val check : L23RR.term -> L23RR.term
	    
end = struct

  structure L = L23RR

 
  fun hasUniqueLabels (records: (string * L.term) list) : bool =
    let
      (* Function to check if a label is in a list of labels *)
      fun isLabelInList (label, []) = false
        | isLabelInList (label, x::xs) = 
            if label = x then true
            else isLabelInList (label, xs)

      (* Recursive function to check labels at the current level and nested levels *)
      fun checkLabels [] = true
        | checkLabels ((label, L.Record newRec)::xs) = 
            if isLabelInList (label, List.map #1 xs) then false
            else hasUniqueLabels(newRec) andalso checkLabels(xs)
        | checkLabels ((label, _)::xs) = 
            if isLabelInList (label, List.map #1 xs) then false
            else checkLabels xs
    in
      checkLabels records
    end


  fun checkRecord records =
    if List.null records then
      raise Fail "null record"
    else if not (hasUniqueLabels records) then
      raise Fail "wrong record"
    else
      List.map (fn (label, term) => (label, check term)) records

  and check term = 
    case term of 
        L.Int _ => term
      | L.True => term
      | L.False => term
      | L.Unit => term
      | L.Var _ => term
      | L.Lam (x, t, term) => L.Lam (x, t, check term)
      | L.App (t1, t2) => L.App (check t1, check t2)
      | L.Fix t => L.Fix (check t)
      | L.Let (x, t1, t2) => L.Let (x, check t1, check t2)
      | L.Cond (t1, t2, t3) => L.Cond (check t1, check t2, check t3)
      | L.Add (t1, t2) => L.Add (check t1, check t2)
      | L.Sub (t1, t2) => L.Sub (check t1, check t2)
      | L.Mul (t1, t2) => L.Mul (check t1, check t2)
      | L.Eq (t1, t2) => L.Eq (check t1, check t2)
      | L.LessThan (t1, t2) => L.LessThan (check t1, check t2)
      | L.Not t => L.Not (check t)
      | L.Record records => L.Record (checkRecord records)
      | L.Select (label, t) => L.Select (label, check t)

end

    (* | Record of (string * term) list
       | Select of string * term *)
