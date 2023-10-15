structure Desugared = struct

  datatype term
    = Zero
    | Succ of term
    | Add of term * term
    | Subtract of term * term
    | Less of term * term
    | Eq of term * term
    | Cond of term * term * term
    | Pair of term * term
    | First of term
    | Second of term
		  
end
