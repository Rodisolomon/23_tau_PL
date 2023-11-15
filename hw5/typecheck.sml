structure TypeCheck : sig

(* return true if the first type is a subtype of the second *)
  val subty : Type.typ * Type.typ -> bool

(* for commonSupertype, use the s function from the PDF *)
(* if there isn't a common supertype, return NONE *)
  val commonSupertype : Type.typ * Type.typ -> Type.typ option

  (* val internalTypeof:  TypeEnv.env * L23RR.term -> Type.typ *)
  val typeof : L23RR.term -> Type.typ
							
end = struct

  structure L = L23RR
  structure T = Type
  structure E = TypeEnv
		  
fun subty (a, b) =
  case (a, b) of

    (* Function: Contravariant in arguments and return types *)
    (Type.Function (arg1, ret1), Type.Function (arg2, ret2)) =>
      subty (arg2, arg1) andalso subty (ret1, ret2)

    (* Record: width, depth, and permutation subtyping (doesn't need explicit implementation?) *)
    | (Type.Record aFields, Type.Record bFields) => (* two Fields two lists *)
      let
        val aFieldNames = map (fn (name, _) => name) aFields (* return a new list of labels in a *)
        val bFieldNames = map (fn (name, _) => name) bFields (* ... in b *)

        (* width what b has, a must has aswell*)
        fun allFieldsInA () = 
          List.all (fn bField => List.exists (fn aField => aField = bField) aFieldNames) bFieldNames
        
        (* depth - recursion *)
        fun fieldsAreSubtypes () = List.all (
          fn (bName, bType) => 
            case List.find (fn (aName, _) => aName = bName) aFields of
              SOME (_, aType) => subty (aType, bType)
            | NONE => false
        ) bFields
      in
        allFieldsInA () andalso fieldsAreSubtypes ()
      end

    | (Type.Int, Type.Int) => true
    | (Type.Bool, Type.Bool) => true
    | (Type.Unit, Type.Unit) => true
    | (_, _) => if a = b then true else false

  fun commonSupertype (a, b) = 
    if subty (a, b) then
      SOME(b)
    else if subty (b, a) then
      SOME(a)
    else NONE

  fun typeof term = 
    let
      fun handleArith (ev, t1, t2) = 
        (case (internalTypeof (ev, t1), internalTypeof (ev, t2)) of
          (T.Int, T.Int) => T.Int
          | _ => raise Fail "arithmetic term should always be int")
      and internalTypeof (ev, term) = 
        case term of
            L.Int _ => T.Int
          | L.True => T.Bool
          | L.False => T.Bool
          | L.Unit => T.Unit
          | L.Var x => 
            (case TypeEnv.lookup (ev, x) of
              SOME tau => tau
            | NONE => raise Fail ("Unbound variable: " ^ x))
          | L.Lam (x, tau1, t1) => 
            let
              val eVExtended = TypeEnv.extend (ev, x, tau1)
            in
              T.Function (tau1, internalTypeof (eVExtended, t1))
            end
          | L.App (t1, t2) => 
            (case internalTypeof (ev, t1) of
              T.Function (tauArg, tauRes) =>
                if subty (internalTypeof (ev, t2), tauArg)
                  then tauRes
                else raise Fail "Type mismatch in function application"
              | _ => raise Fail "Applied term is not a function")
          | L.Fix t1 => 
            (case internalTypeof (ev, t1) of
              T.Function (tau1, tau2) =>
                if tau1 = tau2 then tau1
                else raise Fail "Type mismatch in fixpoint"
            | _ => raise Fail "Fixpoint of non-function")
          | L.Let (x, t1, t2) => 
            let
              val tau1 = internalTypeof (ev, t1)
              val gammaExtended = TypeEnv.extend (ev, x, tau1)
            in
              internalTypeof (gammaExtended, t2)
            end
          | L.Cond (t1, t2, t3) => 
            (case (internalTypeof (ev, t1), internalTypeof (ev, t2), internalTypeof (ev, t3)) of
              (T.Bool, typeB, typeC) => 
                (case commonSupertype (typeB, typeC) of
                  SOME(A) => A
                 | _ => raise Fail "there's no common supertype")
              | _ => raise Fail "wrong type for Cond operator")
          | L.Add (t1, t2) => 
            handleArith (ev, t1, t2)
          | L.Sub (t1, t2) =>
            handleArith (ev, t1, t2)
          | L.Mul (t1, t2) => 
            handleArith (ev, t1, t2)
          | L.Eq (t1, t2) => 
            (case (internalTypeof (ev, t1), internalTypeof (ev, t2)) of
              (T.Int, T.Int) => T.Bool
              | _ => raise Fail "equal term should always be int")
          | L.LessThan (t1, t2) => 
            (case (internalTypeof (ev, t1), internalTypeof (ev, t2)) of
              (T.Int, T.Int) => T.Bool
              | _ => raise Fail "less than term should always be int")
          | L.Not t1 => 
            (case internalTypeof (ev, t1) of
              T.Bool => T.Bool
              | _ => raise Fail "not term should always be bool")
          | L.Record pairs => 
            T.Record (List.map (fn (l, t) => (l, internalTypeof (ev, t))) pairs)
          | L.Select (label, t1) => 
            (case internalTypeof (ev, t1) of
              T.Record fields =>
                (case List.find (fn (l, _) => l = label) fields of
                    SOME (_, tau) => tau
                  | NONE => raise Fail ("Label " ^ label ^ " not found in record"))
            | _ => raise Fail "Selection from non-record type")          
    in
      internalTypeof (TypeEnv.empty, term)
    end
	    
end
