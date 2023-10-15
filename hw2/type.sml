structure Type = struct

(* The odd spelling of the word type here is due to the fact that
   "type" has meaning already in SML (i.e., it's a predefined
   syntactic token), and it is not available for naming anything.
 *)

  datatype typ
    = Nat
    | Bool
    | Unit
    | Product of typ * typ

end
