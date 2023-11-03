structure VarSet :> sig

  type set

  val empty : set
  val mem   : string * set -> bool
  val ins   : string * set -> set
  val rem   : string * set -> set
  val union : set * set -> set

end = struct

  type set = string list

  val empty : string list = []

  fun mem (x, []) = false
    | mem (x, y::ys) = x=y orelse mem (x, ys)

  fun ins (x, s) = if mem (x, s) then s else x::s

  fun rem (x, []) = []
    | rem (x, y::ys) = if x=y then ys else y::(rem (x, ys))

  fun union ([], set2) = set2
    | union (x::xs, set2) = union (xs, ins (x, set2))
                      
end