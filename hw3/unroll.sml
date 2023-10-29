structure Unroll : sig
  type dictionary
  val unroll : Sweetl.prog -> Sweetl.term
  val addToDictionary : string * Sweetl.term * dictionary -> dictionary
  val lookup : string * dictionary -> Sweetl.term option
  val expandTerm : Sweetl.term * dictionary -> Sweetl.term
  val expandList : ((string * Sweetl.term) list) * dictionary -> dictionary
  val unrollHelper : ((string * Sweetl.term) list) * Sweetl.term -> Sweetl.term
end = struct

  structure S = Sweetl
  type dictionary = (string * S.term) list

  fun addToDictionary (key, value, dict) = (key, value)::dict

  fun lookup (key, dict) =
    case List.find (fn (k, _) => k = key) dict of
      NONE => NONE
    | SOME (_, v) => SOME v

  fun expandTerm (term, curDict) = 
    let
      fun lookupAndExpand abbreviation =
        case lookup (abbreviation, curDict) of
          NONE => raise Fail "abbr not exist"
        | SOME expandedTerm => expandedTerm
    in
      case term of  
        S.Var s => S.Var s
      | S.App(t1, t2) => S.App(expandTerm(t1, curDict), expandTerm(t2, curDict))
      | S.Lam(s, t) => S.Lam(s, expandTerm(t, curDict))
      | S.Nat n => S.Nat n
      | S.Tru => S.Tru
      | S.Fls => S.Fls
      | S.ID str => S.ID str
      | S.Abbr str => lookupAndExpand str
    end

fun expandList ([], dict) = dict
  | expandList ((x::xs), dict) = 
    let
      val (key, term) = x
      val newTerm = expandTerm(term, dict)
    in
      let
        val newDict = addToDictionary (key, newTerm, dict)
      in
        expandList (xs, newDict)
      end
    end

  fun unrollHelper (list, term) = 
    let 
      val initialDict = []
      val dict = expandList(list, initialDict)
    in
      expandTerm(term, dict)
    end

  fun containsAbbr (S.Abbr _) = true
    | containsAbbr (S.App (t1, t2)) = containsAbbr t1 orelse containsAbbr t2
    | containsAbbr (S.Lam (_, t)) = containsAbbr t
    | containsAbbr _ = false;

  fun unroll (S.Prog([], term)) = if containsAbbr term then raise Fail "abbr not exist" else term
    | unroll (S.Prog(list, term)) = unrollHelper(list, term)
end


(* p ::= :x = t ; p // abbreviations (bindings)
    | t
    
t ::= x     // variables
    | (t t) // application
    | [x t] // abstraction
    | n     // Church naturals
    | @t    // Church true
    | @f    // Church false
    | &x    // identity functions id
    | :x    // abbreviations (uses) *)

(* to have been expanded away. For example, consider this
not-yet-unrolled program:

  :ab = [a b];
  :cd = [c d];
  (:ab :cd)

  :ab = [a b];
  :cd = [c d];
  (:ab :cd)
  <<<< [("ab", term.Lam("a", term.Var("b"))), ("cd", term.Lam("c", term.Var("d")))], 
  term.App(term.Abbr("ab"), term.Abbr("cd")) >>>>

After unrolling abbreviations, it is this:

  ([a b] [c d]) *)