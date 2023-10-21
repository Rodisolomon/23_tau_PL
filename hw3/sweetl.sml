structure Sweetl = struct

  datatype term
    = Var of string
    | App of term * term
    | Lam of string * term
    | Nat of int
    | Tru
    | Fls
    | ID of string    (* convention: string does not include the & *)
    | Abbr  of string (* convention: string does not include the : *)

  datatype prog
    = Prog of (string * term) list * term
      (* convention: strings do not include the $ *)
				       
  fun tos (Prog (abbs, t)) =
    let
      fun tm (Var x) = x
	| tm (App (t1, t2)) = "(" ^ tm t1 ^ " " ^ tm t2 ^ ")"
	| tm (Lam (x, t1)) = "[" ^ x ^ " " ^ tm t1 ^ "]"
	| tm (Nat n) = Int.toString n
	| tm Tru = "@t"
	| tm Fls = "@f"
	| tm (ID x) = "&" ^ x
	| tm (Abbr a) = ":" ^ a
      and pr ([], main) = tm main
	| pr ((a,t)::abbs, main) =
	    ":" ^ a ^ " = " ^ tm t ^ ";\n" ^ pr (abbs, main)
    in
      pr (abbs, t)
    end
				       
end
