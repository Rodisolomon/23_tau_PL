structure ULC = struct

  datatype term
    = Var of string
    | App of term * term
    | Lam of string * term

  fun tos (Var x) = x
    | tos (App (t1, t2)) = "(" ^ tos t1 ^ " " ^ tos t2 ^ ")"
    | tos (Lam (x, t1)) = "[" ^ x ^ " " ^ tos t1 ^ "]"
			
end
